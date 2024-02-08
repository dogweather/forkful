---
title:                "Читання текстового файлу"
aliases:
- uk/elm/reading-a-text-file.md
date:                  2024-01-20T17:54:17.028361-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і чому?
Читання текстового файлу – це процес отримання даних з файлу, збереженого на вашому диску. Програмісти роблять це, щоб обробити збережені дані, налаштувати програму, або завантажити конфігурації.

## Як це зробити:
Elm напряму не має здатності читати файли через браузерну безпеку, але ви можете использовати Port для інтеграції з JavaScript.

```Elm
port module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

port readFile : () -> Cmd msg
port fileRead : (String -> msg) -> Sub msg

type Msg
    = ReadFile
    | FileRead String

type alias Model =
    { content : String
    }

init : Model
init =
    { content = ""
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReadFile ->
            (model, readFile ())

        FileRead content ->
            ({ model | content = content }, Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick ReadFile ] [ text "Читати файл" ]
        , div [] [ text model.content ]
        ]

main =
    Html.program
        { init = (init, Cmd.none)
        , update = update
        , subscriptions = \_ -> fileRead FileRead
        , view = view
        }
```

JavaScript для інтеграції з портами Elm:

```javascript
app.ports.readFile.subscribe(function() {
    var selectedFile = document.getElementById('file-input').files[0];
    var reader = new FileReader();
    
    reader.onload = function(event) {
        app.ports.fileRead.send(event.target.result);
    };
    
    reader.readAsText(selectedFile);
});
```

HTML елемент для вибору файлу:

```html
<input type="file" id="file-input" />
```

## Поглиблений розгляд:
Elm був створений для надання надійності та продуктивності frontend-розробці. Читання файлів локально не підтримується безпосередньо через пріоритет безпеки й простоту. Історично Elm вибрали шлях взаємодії з JavaScript для таких задач через Ports. Альтернативою є використання File Reader API в JavaScript і передача даних через порт у Elm. Це рішення дозволяє обробляти файли безпосередньо в браузері.

## Дивіться також:
- [Офіційні порти Елма](https://guide.elm-lang.org/interop/ports.html) - навчання, як використовувати порти для двосторонньої взаємодії з JavaScript.
- [File Reader API](https://developer.mozilla.org/en-US/docs/Web/API/FileReader) - опис API для читання змісту файлів у веб-браузерах.
