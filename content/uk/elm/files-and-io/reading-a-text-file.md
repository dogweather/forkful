---
date: 2024-01-20 17:54:17.028361-07:00
description: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443 \u2013 \u0446\u0435\
  \ \u043F\u0440\u043E\u0446\u0435\u0441 \u043E\u0442\u0440\u0438\u043C\u0430\u043D\
  \u043D\u044F \u0434\u0430\u043D\u0438\u0445 \u0437 \u0444\u0430\u0439\u043B\u0443\
  , \u0437\u0431\u0435\u0440\u0435\u0436\u0435\u043D\u043E\u0433\u043E \u043D\u0430\
  \ \u0432\u0430\u0448\u043E\u043C\u0443 \u0434\u0438\u0441\u043A\u0443. \u041F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\
  \u0442\u044C \u0446\u0435, \u0449\u043E\u0431 \u043E\u0431\u0440\u043E\u0431\u0438\
  \u0442\u0438 \u0437\u0431\u0435\u0440\u0435\u0436\u0435\u043D\u0456 \u0434\u0430\
  \u043D\u0456,\u2026"
lastmod: '2024-03-13T22:44:49.178615-06:00'
model: gpt-4-1106-preview
summary: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\
  \u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443 \u2013 \u0446\u0435\
  \ \u043F\u0440\u043E\u0446\u0435\u0441 \u043E\u0442\u0440\u0438\u043C\u0430\u043D\
  \u043D\u044F \u0434\u0430\u043D\u0438\u0445 \u0437 \u0444\u0430\u0439\u043B\u0443\
  , \u0437\u0431\u0435\u0440\u0435\u0436\u0435\u043D\u043E\u0433\u043E \u043D\u0430\
  \ \u0432\u0430\u0448\u043E\u043C\u0443 \u0434\u0438\u0441\u043A\u0443. \u041F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\
  \u0442\u044C \u0446\u0435, \u0449\u043E\u0431 \u043E\u0431\u0440\u043E\u0431\u0438\
  \u0442\u0438 \u0437\u0431\u0435\u0440\u0435\u0436\u0435\u043D\u0456 \u0434\u0430\
  \u043D\u0456,\u2026"
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

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
