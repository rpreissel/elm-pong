module Key exposing (..)


type Key
    = LeftPaddleUp
    | LeftPaddleDown
    | RightPaddleUp
    | RightPaddleDown
    | StartPause
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        38 ->
            RightPaddleUp

        40 ->
            RightPaddleDown

        83 ->
            LeftPaddleUp

        88 ->
            LeftPaddleDown

        32 ->
            StartPause
              
        _ ->
            Unknown
