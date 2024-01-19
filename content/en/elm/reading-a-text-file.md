---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file means accessing the string content of a file. These might be used for machine learning purposes, or to grab live data for a website — anything where you need to utilize off code data. 

## How to:

In Elm, we don't use your general purpose I/O methods you'd find in other languages. Instead, we leverage ports for communication. Here's an example of how we receive a string from JavaScript:

```Elm
port module Main exposing (..)

port receiveFileContents : (String -> msg) -> Sub msg

type Msg = Contents String

subscriptions : Model -> Sub Msg
subscriptions _ = receiveFileContents Contents

port sendOpenFileRequest : () -> Cmd msg

openFile : Cmd Msg
openFile = sendOpenFileRequest ()

main =
    Html.program
        { init = ( "Awaiting file...", Cmd.none )
        , view = \_ -> text "Awaiting file..."
        , update = \_ _ -> ( "File content received!", Cmd.none )
        , subscriptions = subscriptions
        }
```
You simply request an Elm port to read the file from JS, then send back the file contents. 

## Deep Dive

Elm's approach to I/O operations maintains its strong emphasis on pure functions. Traditional file I/O operations usually rely on side effects and can create unpredictability. Elm addresses these issues by using its "port" system for communicating with the outside world, like JavaScript. The process is more verbose but guarantees safety and predictability.

There are also alternatives to this in Elm, such as using Http to fetch text files over a network or even using WebSockets.

The implementation details of the "port" system in Elm works seamlessly. JavaScript initiates the process of reading a file, and then it sends the content back to Elm through signals, keeping the entire process free of side effects.

## See Also

1. Official Elm guide on interop with JavaScript: https://guide.elm-lang.org/interop/
2. Article on ports and subscriptions in Elm: https://elmprogramming.com/ports.html
3. Elm's Http module for network requests: https://package.elm-lang.org/packages/elm/http/latest/