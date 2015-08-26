module Audio (
  play
  ) where
{-|

#Play pause
@docs play
-}


import Native.Audio
import Task exposing (Task)

type alias ElementSelector = String
type alias NativeEvent = String
type alias EventName = String


{-| Play initiate play on an audio element, given by ELementSelector
-}
play : ElementSelector -> Task String String
play = Native.Audio.play
