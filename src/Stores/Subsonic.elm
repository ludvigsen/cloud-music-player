module Subsonic (
  root,
  dir
  ) where

{-|

#Subsonic song

#Root
@docs root

#Dir
@docs dir
-}

--import GenericModels exposing (Song, Directory(..))
import GenericModels exposing (Song, Directory(Directory))
import Json.Decode as Json exposing ((:=))
import Http exposing (..)
import Task exposing (..)


type alias Directory' =
  {
    id : String,
    title : String
  }


type alias Index =
  { 
    name : String,
    artists : List Directory'
  }


rootList : Json.Decoder (List Directory)
rootList = 
  Json.map 
  (\artistList -> List.map (\artist -> Directory {id = artist.id, title = artist.title, parent = Nothing, songChildren = [], directoryChildren = []}) artistList) 
  (Json.map (\list -> List.concat (List.map (\li -> li.artists) list))
    (Json.at ["subsonic-response", "indexes", "index"] <| 
    Json.list <|
    Json.object2 Index
    ("name" := Json.string)
    ("artist" := (Json.list <|
      Json.object2 Directory'
      ("id" := Json.string)
      ("name" := Json.string)))))

{-|
-}
root : String -> String -> String -> Task String (List Directory)
root server user password =
  Http.get rootList 
  (Http.url ("http://"++server++"/rest/getIndexes.view") [
    ("u", user),
    ("p",password),
    ("v","1.12.0"),
    ("c","subsonic-client"),
    ("f","json")])
  |> Task.mapError toString


type alias Directory'' = {
  id: String, name: String, child: List Directory'''
}


type alias Directory''' = {
  id: String, 
  parent: Maybe String, 
  isDir: Bool, 
  title: String,
  artist: Maybe String,
  track: Maybe Int,
  year: Maybe Int,
  genre: Maybe String
}
{-
    id: String,
    title: String,
    artist: Maybe String,
    track: Maybe Int,
    year: Maybe Int,
    genre: Maybe String
  -}
toSong : Directory''' -> Song
toSong dir = {
  id =  dir.id,
  title = dir.title,
  artist = dir.artist,
  track = dir.track,
  year = dir.year,
  genre = dir.genre
  }

toDir :  Directory''' -> Directory
toDir dir = Directory {
  id = dir.id,
  title = dir.title,
  parent = dir.parent,
  songChildren = [],
  directoryChildren = []
  }

dir' : Directory -> Json.Decoder Directory
dir' (GenericModels.Directory originalDir) = 
  Json.map (\list ->
    Directory {
      id = originalDir.id,
      title = originalDir.title,
      parent = originalDir.parent,
      songChildren = List.map toSong (List.filter (\c -> not c.isDir) list),
      directoryChildren = List.map toDir (List.filter (\c -> c.isDir) list)})
    (Json.map (\dir -> dir.child)
       (Json.at ["subsonic-response", "directory"] <|
       Json.object3 Directory''
       ("id" := Json.string)
       ("name" := Json.string)
       ("child" := (Json.list <| 
          Json.object8 Directory'''
          ("id" := Json.string)
          (Json.maybe ("parent" := Json.string))
          ("isDir" := Json.bool)
          ("title" := Json.string)
          (Json.maybe ("artist" := Json.string))
          (Json.maybe ("track" := Json.int))
          (Json.maybe ("year" := Json.int))
          (Json.maybe ("genre" := Json.string))))))


{-|
-}
dir : String -> String -> String -> Directory -> Task String Directory
dir server user password (GenericModels.Directory originalDir) = 
  Http.get (dir' (GenericModels.Directory originalDir))
  (Http.url ("http://"++server++"/rest/getMusicDirectory.view") [
    ("u", user),
    ("p",password),
    ("id", originalDir.id),
    ("v","1.12.0"),
    ("c","subsonic-client"),
    ("f","json")])
  |> Task.mapError toString

-- Convenient for decoding large JSON objects
--andMap : Json.Decoder (a -> b) -> Json.Decoder a -> Json.Decoder b
--andMap = Json.object2 (<|)
