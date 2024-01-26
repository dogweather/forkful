---
title:                "Lectura de un archivo de texto"
date:                  2024-01-20T17:54:34.325892-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lectura de un archivo de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Leer un archivo de texto en programación es el proceso de cargar y procesar el contenido de un archivo en forma de texto. Los programadores lo hacen para obtener datos, configuraciones o simplemente para importar información que necesitan en sus aplicaciones.

## Cómo hacerlo:

En Elm, leer un archivo de texto implica trabajar con HTML5 y los eventos `File` y `FileReader`. Elm no permite leer archivos directamente por motivos de seguridad y simplicidad, pero podemos hacerlo interactuando con el DOM a través de `ports`.

```Elm
port module Main exposing (..)

import Browser
import Html exposing (Html, button, text, input)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Ports exposing (fileReader)

type alias Model =
    { content : String }

type Msg
    = ReadFile
    | FileSelected String

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

init : () -> (Model, Cmd Msg)
init _ =
    ({ content = "" }, Cmd.none)

view : Model -> Html Msg
view model =
    Html.div []
        [ input [ Html.Events.onInput FileSelected, Html.Attributes.type_ "file" ] []
        , button [ onClick ReadFile ] [ text "Leer Archivo" ]
        , Html.div [] [ text model.content ]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReadFile ->
            (model, fileReader [])

        FileSelected content ->
            ({ model | content = content }, Cmd.none)

port fileReader : List (String -> msg) -> Cmd msg
```

En tu archivo `index.html`, asegúrate de conectar los ports:

```Javascript
var app = Elm.Main.init({
  node: document.getElementById('elm')
});

// JavaScript code to handle file reading
app.ports.fileReader.subscribe(function() {
  var node = document.querySelector('input[type="file"]');
  if(node.files.length > 0) {
    var reader = new FileReader();
    reader.onload = function(e) {
      var contents = e.target.result;
      app.ports.fileReader.send(contents);
    };
    reader.readAsText(node.files[0]);
  }
});
```

Ahora al seleccionar un archivo y hacer clic en "Leer Archivo", el contenido del archivo será mostrado en la aplicación.

## Profundización:

Históricamente, Elm ha optado por mantener la seguridad y simplicidad, alejándose de APIs que puedan presentar riesgos o complejidad excesiva. Así que la lectura directa de archivos no es algo que Elm maneje nativamente.

Alternativas en JavaScript como `FileReader` y la API `fetch` pueden ser enganchadas a través de `ports` en Elm. Esto hace que Elm sea capaz de manejar archivos, delegando la responsabilidad al JS, donde la manipulación de archivos es más flexible.

Es crucial entender el sistema de `ports` para integrar Elm con JavaScript, ya que esto permite que acciones fuera del alcance de Elm se manejen de forma segura y controlada.

## Véase También:

- [Elm Guide on Interop with JavaScript](https://guide.elm-lang.org/interop/)
- [MDN documentation on the FileReader API](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)
