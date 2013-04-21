{-# OPTIONS_GHC -Wall #-}
{-# Language ForeignFunctionInterface #-}
{-# Language EmptyDataDecls #-}

module OISInputManager ( InputManager
                       , KeyEvent(..)
                       , Key(..)
                       , newInputManager
                       , destroyInputManager
                       , capture
                       , copyKeyStates
                       , getPressedKeys
                       , getKeyEvents
                       ) where

import Foreign.Ptr ( Ptr )
import Foreign.C.Types ( CChar, CInt(..), CUInt )
import Foreign.Marshal ( malloc, free )
import Foreign.Marshal.Array ( mallocArray, peekArray )
import Foreign.Storable ( peek )

import Graphics.Ogre.Types ( RenderWindow(..) )

import Key( KeyEvent(..), Key(..) )

data InputManagerRaw
newtype InputManager = InputManager (Ptr InputManagerRaw)

foreign import ccall unsafe "newInputManager" c_newInputManager
  :: Ptr RenderWindow -> IO (Ptr InputManagerRaw)
foreign import ccall unsafe "destroyInputManager" c_destroyInputManager
  :: Ptr InputManagerRaw -> IO ()
foreign import ccall unsafe "capture" c_capture
  :: Ptr InputManagerRaw -> IO ()
foreign import ccall unsafe "copyKeyStates" c_copyKeyStates
  :: Ptr InputManagerRaw -> Ptr CChar -> IO ()
foreign import ccall unsafe "popKeyStack" c_popKeyStack
  :: Ptr InputManagerRaw -> Ptr CUInt -> Ptr CUInt -> Ptr CInt -> IO CInt

newInputManager :: RenderWindow -> IO InputManager
newInputManager (RenderWindow ptr) = do
  imRaw <- c_newInputManager ptr
  return (InputManager imRaw)

destroyInputManager :: InputManager -> IO ()
destroyInputManager (InputManager imRaw) = c_destroyInputManager imRaw

capture :: InputManager -> IO ()
capture (InputManager imRaw) = c_capture imRaw

copyKeyStates :: InputManager -> IO [CChar]
copyKeyStates (InputManager imRaw) = do
  let len = 256
  keys <- mallocArray len
  c_copyKeyStates imRaw keys
  peekArray len keys

getPressedKeys :: InputManager -> IO [Key]
getPressedKeys im = do
  ks <- copyKeyStates im
  let go :: Int -> [CChar] -> [Key] -> [Key]
      go n (0:xs) acc = go (n+1) xs acc
      go n (1:xs) acc = go (n+1) xs ((toEnum n):acc)
      go _ [] acc = acc
      go _ (ret:_) _ =
        error $ "the \"impossible\" happened, getPressedKeys got non-zero return code: "++show ret
  return $ go 0 ks []

convertKey :: CUInt -> CUInt -> CInt -> KeyEvent
convertKey keycode _text 1 = Pressed (toEnum (fromIntegral keycode))
convertKey keycode _text 0 = Released (toEnum (fromIntegral keycode))
convertKey _ _ ret =
  error $ "the \"impossible\" happened, convertKey got non-zero return code: "++show ret

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
  keys <- getMoar []

  free keycode
  free text
  free pressed
  
  return keys
