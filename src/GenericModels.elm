module GenericModels (
  Song,
  Directory(..)
  ) where
{-|

#PSong
@docs Song
-}

type alias Song =
  {
    id: String,
    title: String,
    artist: Maybe String,
    track: Maybe Int,
    year: Maybe Int,
    genre: Maybe String
  }

type Directory = Directory
  {
    id: String,
    title: String,
    parent: Maybe String,
    songChildren: List Song,
    directoryChildren: List Directory
  }
