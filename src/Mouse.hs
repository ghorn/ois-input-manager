{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveDataTypeable #-}

module Mouse ( MouseEvent(..)
             , MouseButton(..)
             , MouseAxis(..)
             , MouseAxes(..)
             ) where

import Data.Typeable ( Typeable )
import Data.Data ( Data )

-- | absolute and relative pointer positions
data MouseAxis = MouseAxis Int Int deriving (Show, Eq, Ord, Data, Typeable)
data MouseAxes = MouseAxes { axisX :: MouseAxis
                           , axisY :: MouseAxis
                           , axisZ :: MouseAxis
                           } deriving (Show, Eq, Ord, Data, Typeable)

data MouseEvent = ButtonPressed MouseButton
                | ButtonReleased MouseButton
                | MouseMoved MouseAxes
                deriving (Show, Eq, Ord, Data, Typeable)

data MouseButton = MB_Left
                 | MB_Right
                 | MB_Middle
                 | MB_Button3
                 | MB_Button4
                 | MB_Button5
                 | MB_Button6
                 | MB_Button7
                 deriving (Enum, Show, Eq, Ord, Data, Typeable)
