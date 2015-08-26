module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2, lazy3 )
import Http exposing (..)
import Json.Decode as Json exposing ((:=))
import Signal exposing (Signal, Address)
import String
import Window
import Result
import Task exposing (..)
import Audio
import GenericModels exposing (Song, Directory)
import Subsonic


--MODEL
type alias Model =
  { server : String,
    user : String,
    password : String,
    selectedApi: Api,
    dirs : List Directory,
    root : List Directory,
    error : Maybe String,
    playlist: List Song,
    currentSong: Int
  }


emptyModel : Model
emptyModel = 
  { server   = "tny.io:4040",
    user     = "admin",
    password = "S0uThP4rK",
    selectedApi = Subsonic,
    dirs = [],
    root = [],
    error = Nothing,
    playlist = [],
    currentSong = 0
  }


type Action = NoOp | 
              Error String |
              ChangeServer String | 
              ChangeUser String | 
              ChangePassword String | 
              ChangeApi Api |
              NextSong |
              ChangeDirectory (List Directory) |
              Root (List Directory) |
              ClearSongs |
              SelectSong Int |
              AddSongs (List Song)


type Request = GetRoot Model | GetDir Model Directory | EmptyRequest


-- Convenient for decoding large JSON objects
andMap : Json.Decoder (a -> b) -> Json.Decoder a -> Json.Decoder b
andMap = Json.object2 (<|)


type Api = Subsonic | NoApi


makeRequest : Request -> Task String Action
makeRequest req = 
  case req of
    GetRoot model ->
      case model.selectedApi of 
        Subsonic -> 
          Task.map (\list -> Root list ) (Subsonic.root model.server model.user model.password)
        NoApi -> Task.succeed NoOp
    GetDir model originalDir ->
      case model.selectedApi of 
        Subsonic -> 
          Task.map (\dir -> ChangeDirectory [dir] ) (Subsonic.dir model.server model.user model.password originalDir)
        NoApi -> Task.succeed NoOp
    EmptyRequest -> Task.succeed NoOp


type AudioControl = AudioControlNoOp | Play String

controlAudio : AudioControl -> Task String String
controlAudio action = 
  case action of
    Play s -> Audio.play s
    AudioControlNoOp   -> Task.succeed "NoOp"


audioControlS : Signal.Mailbox AudioControl
audioControlS =
  Signal.mailbox AudioControlNoOp


port audioEvents : Signal String

audioEventsHandler : String -> Task x ()
audioEventsHandler event =
  case event of
    "ended" -> 
      (Signal.send actions.address NextSong) `andThen` 
      (\_ -> (Signal.send audioControlS.address (Play "#player")))
    _ -> Signal.send audioControlS.address (Play event)

port runner : Signal (Task x ())
port runner = Signal.map audioEventsHandler audioEvents

port audioControl : Signal String
port audioControl = Signal.map controlAudio audioControlS.signal |> Signal.map (\_ -> "")

port request : Signal (Task Http.Error ())
port request = 
  Signal.map makeRequest query.signal |> Signal.map (\task -> andThen (Task.toResult task) sendAction)

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
    ChangeApi        newApi      -> {model | selectedApi <- newApi}
    ChangeDirectory  dirList     -> {model | dirs <- dirList}
    AddSongs         songs       -> {model | playlist <- (model.playlist ++ songs)}
    Root             newRoot     -> {model | root <- newRoot}
    ClearSongs                   -> {model | playlist <- []}
    SelectSong       song        -> {model | currentSong <- song}
    NextSong                     -> {model | currentSong <- (model.currentSong + 1) % (List.length model.playlist)}
    Error            message     -> {model | error <- Just message}
    NoOp                         -> {model | error <- Nothing}


indexItem model address (GenericModels.Directory i) =
  li [onClick address (GetDir model (GenericModels.Directory i))]
    [p [] [text i.title]]


indexListEl model address = 
  ul [ class "artist-list" ] (List.map (indexItem model address) model.root)


dirEl model address actionAddress (GenericModels.Directory dir) = 
  li [onClick address (GetDir model (GenericModels.Directory dir))] [
    span [] [text dir.title]
  ]

dirList model address actionAddress albums = 
  ul [class "directory-list"] (List.map (dirEl model address actionAddress) albums)


changeSongThenPlay : Signal.Mailbox Int
changeSongThenPlay = Signal.mailbox 0


changeSongThenPlayHandler : Int -> Task x ()
changeSongThenPlayHandler index =
  (Signal.send actions.address (SelectSong index)) `andThen` 
  (\_ -> (Signal.send audioControlS.address (Play "#player")))


port changeSongThenPlayRunner : Signal (Task x ())
port changeSongThenPlayRunner = 
  Signal.map changeSongThenPlayHandler changeSongThenPlay.signal


playlistEl model address index song = 
  li [
    class (if index == model.currentSong then "active" else ""),
    onClick changeSongThenPlay.address index] [
    span [] [text (toString index)],
    span [] [text song.title]
    ]


playlist model address = 
  div [class "playlist"] [
    h2 [] [text "Playlist"],
    ul [] (List.indexedMap (playlistEl model address) model.playlist),
    p [onClick address ClearSongs] [text "Clear playlist"]
  ]


player model address = 
  let maybeSong = List.head (List.drop model.currentSong model.playlist)
  in
     div [class "player"] [
       span [] [text ("Current song: " ++ toString model.currentSong)],
       (case maybeSong of 
         Just song -> p [] [text ("Now playing: " ++ song.title ++ " id: " ++ (toString song.id))]
         Nothing -> p [] []),
         audio [id "player", controls True, autoplay True, 
                name "media", 
                on "ended" targetValue (\_ -> Signal.message address NextSong),
                on "ended" targetValue (\_ -> Signal.message audioControlS.address (Play "#player"))
         ] [
           case maybeSong of
             Just song ->
               source [type' "audio/mpeg", src ("http://"++model.server++"/rest/stream.view?id="++song.id++"&u="++model.user++"&p="++model.password++"&v=1.12.0&c=subsonic-client&f=json")] []
             Nothing ->
               source [] []
             ],
             p [onClick audioControlS.address (Play "#player")] [text "PLAY"]
           ]


mainArea model address actionAddress =
  div [class "main-area"] [
      (case model.error of
        Just error -> p [] [text error]
        Nothing -> text ""
      ),
      --span [onClick actionAddress (AddSongs model.dirs), class "add-to-playlist"] [text "Add to playlist"]
      --p [] [text ((toString album.songCount) ++ " tracks")],
      --songList model address actionAddress model.dirs,
      dirList model address actionAddress model.dirs,
      div [class "player-area"] [
        player model actionAddress,
        playlist model actionAddress
        ]
    ]

toApi : Json.Decoder Api
toApi = 
  Json.at ["target","value"] Json.string |>
  Json.map (\s -> 
    case s of 
      "Subsonic" -> Subsonic
      _ -> NoApi)

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
      select [
        on "change" toApi (Signal.message address << ChangeApi)
      ] [
          option [ value (toString Subsonic), selected (Subsonic == model.selectedApi)] [text (toString Subsonic)]
        ],
      button [onClick query.address (GetRoot model)] [text "Get json"]
    ]


view : Signal.Address Action -> Model -> Html
view address model = 
  div []
    [
      header model address
    , div [class "body-container"] [
        indexListEl model query.address
      , mainArea model query.address address
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

-- actions from user input
actions : Signal.Mailbox Action
actions =
    Signal.mailbox NoOp


