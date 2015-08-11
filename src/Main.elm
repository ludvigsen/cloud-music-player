import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Http
import Json.Decode as Json exposing ((:=))
import Signal exposing (Signal, Address)
import String
import Window
import Result
import Task exposing (..)

type alias Model =
  { server : String,
    user : String,
    password : String,
    selectedArtist : Maybe DetailedArtist,
    artistList : List Index,
    selectedAlbum: Maybe DetailedAlbum,
    artistId : String,
    albumId : String,
    error : Maybe String
  }

emptyModel : Model
emptyModel = 
  { server   = "192.168.1.20:4040",
    user     = "admin",
    password = "S0uThP4rK",
    selectedArtist = Nothing,
    artistList = [],
    selectedAlbum = Nothing,
    artistId = "",
    albumId = "",
    error = Nothing
  }

type Action = NoOp | 
              Error String |
              ChangeServer String | 
              ChangeUser String | 
              ChangePassword String | 
              IndexList (List Index) |
              ChangeArtist DetailedArtist | 
              ChangeAlbum DetailedAlbum

type Request = IndexListRequest Model | AlbumListRequest Model | AlbumRequest Model | EmptyRequest

--type Result = IndexList (List Index) | Albums (List Album) | EmptyResult

type alias Artist =
  { id : String,
    name : String,
    coverArt : Maybe String,
    albumCount : Int
  }

type alias Index =
  { 
    name : String,
    artists : List Artist
  }

type alias Album =
  {
    name : String,
    songCount: Int
  }

type alias Song =
  {
    id: String,
    parent: String,
    isDir: Bool,
    title: String,
    album: String,
    artist: String,
    track: Int,
    year: Int,
    genre: Maybe String,
    size: Int,
    contentType: String,
    suffix: String,
    transcodedContentType: Maybe String,
    transcodedSuffix: Maybe String,
    duration: Int,
    bitRate: Maybe Int,
    path: String,
    isVideo: Bool,
    created: String,
    albumId: String,
    artistId: String,
    audioType: Maybe String
  }


songDecoder : Json.Decoder Song
songDecoder = Song 
  `Json.map` ("id" := Json.string)
  `andMap` ("parent" := Json.string)
  `andMap` ("isDir" := Json.bool)
  `andMap` ("title" := Json.string)
  `andMap` ("album" := Json.string)
  `andMap` ("artist" := Json.string)
  `andMap` ("track" := Json.int)
  `andMap` ("year" := Json.int)
  `andMap` Json.maybe ("genre" := Json.string)
  `andMap` ("size" := Json.int)
  `andMap` ("contentType" := Json.string)
  `andMap` ("suffix" := Json.string)
  `andMap` Json.maybe ("transcodedContentType" := Json.string)
  `andMap` Json.maybe ("transcodedSuffix" := Json.string)
  `andMap` ("duration" := Json.int)
  `andMap` Json.maybe ("bitRate" := Json.int)
  `andMap` ("path" := Json.string)
  `andMap` ("isVideo" := Json.bool)
  `andMap` ("created" := Json.string)
  `andMap` ("albumId" := Json.string)
  `andMap` ("artistId" := Json.string)
  `andMap` (Json.maybe ("audioType" := Json.string))


artistList : Json.Decoder (List Artist)
artistList = Json.list <| Json.object4 Artist
                          ("id" := Json.string)   
                          ("name" := Json.string)
                          (Json.maybe ("coverArt" := Json.string))
                          ("albumCount" := Json.int)


indexList : Json.Decoder Action
indexList = Json.map (\list -> IndexList list) (
  Json.at ["subsonic-response","artists","index"] <| Json.list <|
  Json.object2 Index
  ("name" := Json.string)
  ("artist" := artistList))

type alias DetailedArtist =
  {
    id : String,
    name : String,
    albumCount: Int,
    album : List DetailedAlbum
  }

emptyArtist : DetailedArtist
emptyArtist = 
  {
    id = "",
    name = "",
    albumCount = 0,
    album = []
  }


type alias DetailedAlbum =
  {
    id : String,
    name : String,
    artist: String,
    artistId: String,
    songCount: Int,
    duration: Int,
    created: String,
    year: Int,
    genre: Maybe String,
    songs: Maybe (List Song)
  }


-- Convenient for decoding large JSON objects
andMap : Json.Decoder (a -> b) -> Json.Decoder a -> Json.Decoder b
andMap = Json.object2 (<|)


albumDecoder : Json.Decoder DetailedAlbum
albumDecoder = DetailedAlbum 
  `Json.map` ("id" := Json.string)
  `andMap` ("name" := Json.string)
  `andMap` ("artist" := Json.string)
  `andMap` ("artistId" := Json.string)
  `andMap` ("songCount" := Json.int)
  `andMap` ("duration" := Json.int)
  `andMap` ("created" := Json.string)
  `andMap` ("year" := Json.int)
  `andMap` (Json.maybe ("genre" := Json.string))
  `andMap` (Json.maybe ("song" := (Json.list <| songDecoder)))

singleAlbumDecoder : Json.Decoder Action
singleAlbumDecoder = Json.map (\album -> ChangeAlbum album)
  (Json.at ["subsonic-response", "album"] <| albumDecoder)

artistDecoder : Json.Decoder Action
artistDecoder = Json.map (\artist -> ChangeArtist artist)
  (Json.at ["subsonic-response", "artist"] <| Json.object4 DetailedArtist
  ("id" := Json.string)
  ("name" := Json.string)
  ("albumCount" := Json.int)
  ("album" := (Json.list <| albumDecoder)))


makeRequest : Request -> Task String Action
makeRequest req = 
  case req of
    IndexListRequest model ->
      Http.get indexList ("http://"++model.server++"/rest/getArtists.view?u="++model.user++"&p="++model.password++"&v=1.12.0&c=subsonic-client&f=json")
      |> Task.mapError toString
    AlbumListRequest model ->
      Http.get artistDecoder ("http://"++model.server++"/rest/getArtist.view?u="++model.user++"&p="++model.password++"&v=1.12.0&c=subsonic-client&f=json&id="++model.artistId)
      |> Task.mapError toString
    AlbumRequest model ->
      Http.get singleAlbumDecoder ("http://"++model.server++"/rest/getAlbum.view?u="++model.user++"&p="++model.password++"&v=1.12.0&c=subsonic-client&f=json&id="++model.albumId)
      |> Task.mapError toString
    EmptyRequest -> Task.succeed NoOp
  

port request : Signal (Task Http.Error ())
port request = 
  Signal.map makeRequest query.signal  |> Signal.map (\task -> andThen (Task.toResult task) sendAction)

sendAction : Result.Result String Action -> Task x ()
sendAction =
    Signal.send (Signal.forwardTo actions.address toAction)

toAction a = 
  case a of
    Ok val -> val
    Err error -> Error error

results : Signal.Mailbox (Maybe Result)
results =
    Signal.mailbox Nothing


query : Signal.Mailbox Request
query =
    Signal.mailbox EmptyRequest


update : Action -> Model -> Model
update action model = 
  case action of
    ChangeServer     newServer   -> {model | server     <- newServer, error <- Nothing}
    ChangeUser       newUser     -> {model | user       <- newUser, error <- Nothing}
    ChangePassword   newPassword -> {model | password   <- newPassword, error <- Nothing}
    IndexList        indexs      -> {model | artistList <- indexs, error <- Nothing}
    ChangeArtist     artist      -> {model | selectedArtist  <- Just artist, artistId <- artist.id, error <- Nothing, selectedAlbum <- Nothing}
    ChangeAlbum      album       -> {model | selectedAlbum  <- Just album, albumId <- album.id, error <- Nothing}
    Error            message     -> {model | error <- Just message}
    NoOp                         -> {model | error <- Nothing}


indexItem address model i =
  li [onClick address (AlbumListRequest {model | artistId <- i.id})]
    [p [] [text i.name]]

indexListEl model address = 
  ul [ class "artist-list" ] (List.foldr (++) [] (List.map (\i -> List.map (indexItem address model) i.artists) model.artistList))

dummyData : Maybe (List Index)
dummyData = Just ([{name = "a", artists = [{id = "1", name = "Test", coverArt = (Just ""), albumCount = 1}]}])

dummyAlbums : List Album
dummyAlbums = [{name = "Test", songCount = 5}]


albumEl model address album = 
  p [onClick address (AlbumRequest {model | albumId <- album.id})] [
    text album.name
  ]

songEl model song =
  p [] [
    a [href ("http://"++model.server++"/rest/stream.view?id="++song.id++"&u="++model.user++"&p="++model.password++"&v=1.12.0&c=subsonic-client&f=json")] [text song.title]
  ]

albumList model address albums = 
  div [class "album-list"] (List.map (albumEl model address) albums)

songList model album =
  case album.songs of
    Just songs -> div [class "song-list"] (List.map (songEl model) songs)
    Nothing -> text ""

mainArea model address =
  div [class "main-area"] [
      h1 [] [text "Test"],
      (case model.error of
        Just error -> p [] [text error]
        Nothing -> text ""
      ),
      (case model.selectedAlbum of
        Just album -> div [] [
          p [] [text album.name],
          p [] [text ((toString album.songCount) ++ " tracks")],
          songList model album
          ]
        Nothing -> (case model.selectedArtist of
          Just artist -> albumList model address artist.album
          Nothing -> text model.artistId
        )
      )
    ]

header model address = 
  div [class "header"]
    [ input [
        on "input" targetValue (Signal.message address << ChangeServer),
        value model.server
      ] [],
      input [
        on "input" targetValue (Signal.message address << ChangeUser),
        value model.user
      ] [],
      input [
        on "input" targetValue (Signal.message address << ChangePassword),
        type' "password",
        value model.password
      ] [],
      button [onClick query.address (IndexListRequest model)] [text "Get json"]
    ]


view : Signal.Address Action -> Model -> Html
view address model = 
  div []
    [
      header model address
    , div [class "body-container"] [
        indexListEl model query.address
      , mainArea model query.address
    ]
    ]

-- INPUTS

-- wire the entire application together
main : Signal Html
main =
    Signal.map (view actions.address) model 

-- manage the model of our application over time
model : Signal Model
model =
  Signal.foldp update emptyModel actions.signal
    --Signal.merge (Signal.foldp update emptyModel actions.signal)
                 --(Signal.foldp update emptyModel results.signal)      

{--resultsSignal : Signal Model
resultsSignal =
    merge (Signal.foldp update emptyModel results.signal)ddkujk--}


-- actions from user input
actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp


