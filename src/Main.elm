module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Input as Input
import Ports.LocalStorage exposing (..)


main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type Model
    = Loading
    | Ready String


type Msg
    = OnTextBoxChange String
    | OnStorageChange
        { key : String
        , value : Maybe String
        }


storageKey : String
storageKey =
    "remember-test-key"


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , addLocalStorageListener storageKey
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    onLocalStorageChange OnStorageChange


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading ->
            case msg of
                OnStorageChange storage ->
                    case storage.value of
                        Just storageValue ->
                            ( Ready storageValue
                            , Cmd.none
                            )

                        Nothing ->
                            ( Ready ""
                            , saveToLocalStorage
                                { key = storageKey
                                , value = ""
                                }
                            )

                _ ->
                    ( model, Cmd.none )

        Ready value ->
            case msg of
                OnTextBoxChange newValue ->
                    ( Ready newValue
                    , saveToLocalStorage
                        { key = storageKey
                        , value = newValue
                        }
                    )

                OnStorageChange storage ->
                    case storage.value of
                        Just storageValue ->
                            ( Ready storageValue
                            , Cmd.none
                            )

                        Nothing ->
                            ( Ready ""
                            , saveToLocalStorage
                                { key = storageKey
                                , value = ""
                                }
                            )


view : Model -> Browser.Document Msg
view model =
    { title = "I remember whatever you tell me..."
    , body =
        [ layout [] <|
            column [ centerX, centerY ]
                [ el [ centerX, centerY ] <|
                    text "I remember whatever you tell me..."
                , case model of
                    Loading ->
                        loadingView

                    Ready record ->
                        readyView record
                ]
        ]
    }


loadingView : Element Msg
loadingView =
    el [ centerX, centerY ] <|
        text "LOADING..."


readyView : String -> Element Msg
readyView value =
    el
        [ centerX
        , centerY
        , padding 12
        , spacing 12
        ]
    <|
        Input.text [ centerX ]
            { label = Input.labelHidden "textbox"
            , onChange = OnTextBoxChange
            , placeholder = Nothing
            , text = value
            }
