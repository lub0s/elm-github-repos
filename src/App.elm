module App exposing (..)

import Github exposing (..)
import Html exposing (..)


main : Program Never Model Action
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
