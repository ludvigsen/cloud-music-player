module GoogleAPI (
  checkAuth
  ) where
{-|

#Google drive api
@docs checkAuth
-}


import Native.GoogleAPI
import Task exposing (Task)

{-| Play initiate play on an audio element, given by ELementSelector
-}
checkAuth : String -> String -> Task String String
checkAuth = Native.GoogleAPI.checkAuth
