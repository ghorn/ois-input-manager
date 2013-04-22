{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}
{-# Language EmptyDataDecls #-}

module OISInputManager ( -- * Input Manager
                         InputManager
                       , newInputManager
                       , destroyInputManager
                       , capture
                         -- * Mouse
                       , getMouseEvents
                       , getMouseState
                       , MouseEvent(..)
                       , MouseButton(..)
                       , MouseAxis(..)
                       , MouseAxes(..)
                         -- * Keyboard
                       , getKeyEvents
                       , getPressedKeys
--                       , getKeyStates
                       , KeyEvent(..)
                       , Key(..)
                       ) where

import Control.Applicative ( (<$>) )
import Data.Maybe ( fromMaybe )
import Foreign.Ptr ( Ptr )
import Foreign.C.String ( newCString )
import Foreign.C.Types ( CChar, CInt(..), CUInt )
import Foreign.Marshal ( malloc, free )
import Foreign.Marshal.Array ( mallocArray, newArray, peekArray )
import Foreign.Storable ( peek )

import Graphics.Ogre.Types ( RenderWindow(..) )

import Key( KeyEvent(..), Key(..), decodeKey )
import Mouse( MouseEvent(..), MouseButton(..), MouseAxis(..), MouseAxes(..) )

data InputManagerRaw
newtype InputManager = InputManager (Ptr InputManagerRaw)

foreign import ccall unsafe "newInputManager" c_newInputManager
  :: Ptr RenderWindow -> Ptr (Ptr CChar) -> Ptr (Ptr CChar) -> CInt -> IO (Ptr InputManagerRaw)
foreign import ccall unsafe "destroyInputManager" c_destroyInputManager
  :: Ptr InputManagerRaw -> IO ()
foreign import ccall unsafe "capture" c_capture
  :: Ptr InputManagerRaw -> IO ()
foreign import ccall unsafe "copyKeyStates" c_copyKeyStates
  :: Ptr InputManagerRaw -> Ptr CChar -> IO ()
foreign import ccall unsafe "popKeyStack" c_popKeyStack
  :: Ptr InputManagerRaw -> Ptr CUInt -> Ptr CUInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "copyMouseState" c_copyMouseState
  :: Ptr InputManagerRaw -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall unsafe "popMouseStack" c_popMouseStack
  :: Ptr InputManagerRaw -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- | Create an input manager for an Ogre window, optionally taking a list of options
newInputManager :: RenderWindow -> Maybe [(String,String)] -> IO InputManager
newInputManager (RenderWindow windowPtr) maybeOpts = do
  let opts = fromMaybe [] maybeOpts
  optKeys <- mapM (newCString . fst) opts
  optVals <- mapM (newCString . snd) opts
  optKeysArray <- newArray optKeys
  optValsArray <- newArray optVals
  imRaw <- c_newInputManager windowPtr
           optKeysArray optValsArray (fromIntegral (length opts))
  free optKeysArray
  free optValsArray
  mapM_ free optKeys
  mapM_ free optVals
  return (InputManager imRaw)

-- | Destroy a window manager
destroyInputManager :: InputManager -> IO ()
destroyInputManager (InputManager imRaw) = c_destroyInputManager imRaw

-- | Capture all for this `InputManager` (call this before querying devices)
capture :: InputManager -> IO ()
capture (InputManager imRaw) = c_capture imRaw

------------------------ KEYBOARD ------------------------
getKeyStates :: InputManager -> IO [CChar]
getKeyStates (InputManager imRaw) = do
  let len = 256
  keys <- mallocArray len
  c_copyKeyStates imRaw keys
  peekArray len keys

-- | Get a list of all keys currently pressed (unbuffered input)
getPressedKeys :: InputManager -> IO [Key]
getPressedKeys im = do
  ks <- getKeyStates im
  let go :: Int -> [CChar] -> [Key] -> [Key]
      go n (0:xs) acc = go (n+1) xs acc
      go n (1:xs) acc = go (n+1) xs ((decodeKey n):acc)
      go _ [] acc = acc
      go _ (ret:_) _ =
        error $ "the \"impossible\" happened, getPressedKeys got non-zero return code: "++show ret
  return $ go 0 ks []

convertKey :: CUInt -> CUInt -> CInt -> KeyEvent
convertKey keycode _text 0 = KeyPressed  (decodeKey (fromIntegral keycode))
convertKey keycode _text 1 = KeyReleased (decodeKey (fromIntegral keycode))
convertKey _ _ ret =
  error $ "the \"impossible\" happened, convertKey got unhandled return code: "++show ret

-- | Get a list of key events since the last `getKeyEvents` was called (buffered input)
getKeyEvents :: InputManager -> IO [KeyEvent]
getKeyEvents (InputManager imRaw) = do
  keycode <- malloc
  text <- malloc
  pressed <- malloc

  let call = c_popKeyStack imRaw keycode text pressed
      getMoar acc = do
        ret <- call
        if ret == 0
          then do keycode' <- peek keycode
                  text'    <- peek text
                  pressed' <- peek pressed
                  getMoar ((convertKey keycode' text' pressed'):acc)
          else return acc
  keyEvents <- getMoar []

  free keycode
  free text
  free pressed

  return keyEvents


------------------  MOUSE ----------------
convertMouse :: [Int] -> CInt -> CInt -> MouseEvent
convertMouse _ buttonId 0 = ButtonPressed  (toEnum (fromIntegral buttonId))
convertMouse _ buttonId 1 = ButtonReleased (toEnum (fromIntegral buttonId))
convertMouse [a1,a2,_,a4,a5,_,a7,a8,_] _ 2 =
  MouseMoved $ MouseAxes { axisX = MouseAxis a1 a2
                         , axisY = MouseAxis a4 a5
                         , axisZ = MouseAxis a7 a8
                         }
convertMouse badAxes _ 2 =
  error $ "the \"impossible\" happened, convertMouse got bad number of axes: "++show badAxes
convertMouse _ _ ret =
  error $ "the \"impossible\" happened, convertMouse got unhandled return code: "++show ret

-- | Get a list of mouse events since the last `getMouseEvents` was called (buffered input)
getMouseEvents :: InputManager -> IO [MouseEvent]
getMouseEvents (InputManager imRaw) = do
  let axesLen = 9
  axes <- mallocArray axesLen
  buttonId <- malloc
  pressedReleasedMoved <- malloc

  let call = c_popMouseStack imRaw axes buttonId pressedReleasedMoved
      getMoar acc = do
        ret <- call
        if ret == 0
          then do axes' <- peekArray axesLen axes
                  buttonId' <- peek buttonId
                  pressedReleasedMoved' <- peek pressedReleasedMoved
                  getMoar ((convertMouse (map fromIntegral axes') buttonId' pressedReleasedMoved'):acc)
          else return acc
  mouseEvents <- getMoar []

  free axes
  free buttonId
  free pressedReleasedMoved

  return mouseEvents

-- | Get the current state of the mouse (unbuffered input)
getMouseState :: InputManager -> IO (MouseAxes, (Int,Int), [MouseButton])
getMouseState (InputManager imRaw) = do
  let axesLen = 9
  axes <- mallocArray axesLen
  width <- malloc
  height <- malloc
  buttons <- malloc

  c_copyMouseState imRaw axes width height buttons
  axes' <- peekArray axesLen axes
  width'  <- fromIntegral <$> peek width
  height' <- fromIntegral <$> peek height
  buttons' <- peek buttons
  let convertAxes :: [Int] -> MouseAxes
      convertAxes [a1,a2,_,a4,a5,_,a7,a8,_] =
        MouseAxes { axisX = MouseAxis a1 a2
                  , axisY = MouseAxis a4 a5
                  , axisZ = MouseAxis a7 a8
                  }
      convertAxes badAxes =
        error $ "the \"impossible\" happened, convertAxes got bad number of axes: "++show badAxes

      convertButtons :: CInt -> [MouseButton]
      convertButtons _ = []

  free axes
  free width
  free height
  free buttons

  return (convertAxes (map fromIntegral axes'), (width', height'), convertButtons buttons')
