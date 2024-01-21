---
title:                "Reading a text file"
date:                  2024-01-20T17:54:03.419339-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading a text file"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file is pulling content from a file that's structured as readable text, rather than binary data. Programmers read text files to access data, configurations, or to import large amounts of text into their applications.

## How to:

Elm is mainly focused on front-end web development, wherein direct file system access is a no-go due to security reasons. Instead, you handle file uploads by users. Here’s how you can read a text file that a user selects:

```Elm
module Main exposing (..)

import Browser
import File exposing (File)
import File.Selector as Selector
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

type alias Model =
    { fileContent : String }

type Msg
    = SelectFile
    | ReceiveFileContent (Result () String)

init : Model
init =
    { fileContent = "" }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SelectFile ->
            (model, fileSelectCmd)

        ReceiveFileContent (Ok content) ->
            ({ model | fileContent = content }, Cmd.none)

        ReceiveFileContent (Err _) ->
            (model, Cmd.none)

fileSelectCmd : Cmd Msg
fileSelectCmd =
    File.select [ Selector.accept "text/*" ] { onDone = ReceiveFileContent }

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SelectFile ] [ text "Select a text file" ]
        , div [] [ text model.fileContent ]
        ]

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
```

Run the code in your browser, click the button, and select a text file. It displays the content in your Elm app.

## Deep Dive

Elm doesn’t read files from the server's file system directly - it wasn't designed for server-side operations. Instead, Elm manages file input through the File API in the browser, typically triggered by a user action, such as a file selection or a drag-and-drop action. It’s a security measure.

In the past, you might have used JavaScript and Node.js to read files server-side, or XMLHttpRequest (XHR) for client-side reading without user interaction. These have different security models and capabilities.

The `File` and `File.Selector` modules in Elm make it fairly smooth to handle file reading in the browser, but remember the "no side effects" philosophy of Elm. That means file reading is tightly controlled, with explicit user actions required. Also, parsing and decoding file content need care to match Elm’s strong typing.

## See Also

- Official Elm File API documentation: https://package.elm-lang.org/packages/elm/file/latest/
- A guide to Elm’s commands and subscriptions (for understanding async operations): https://guide.elm-lang.org/effects/
- Elm Discuss for questions and community interaction: https://discourse.elm-lang.org/