---
title:                "Створення текстового файлу"
aliases: - /uk/elm/writing-a-text-file.md
date:                  2024-01-19
simple_title:         "Створення текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що і чому?

Запис текстового файлу — це процес збереження даних у файл на диску. Програмісти це роблять для збереження результатів, конфігурацій, або обміну даними.

## Як це зробити:

Elm не має прямого способу запису файлів на диск через браузер. Однак, ви можете генерувати файл та пропонувати завантажити його користувачу. Приклад коду:

```Elm
module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import File.Download

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model = String

init : Model
init =
  "Текст для запису у файл."

type Msg
  = Download

update : Msg -> Model -> Model
update _ model =
  model

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Download ] [ text "Завантажити файл" ] ]

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

port module Ports exposing (..)

port download : String -> Cmd msg

sendTextToFile : String -> Cmd msg
sendTextToFile textData =
    port download textData

-- Підключення JS для завантаження файлу
-- Погляньте на секцію "See Also" для посилання на інструкцію.

update msg model =
  case msg of
    Download ->
      ( model, sendTextToFile model )
```

Ваш JS-файл, який взаємодіє з Elm через порти:

```javascript
app.ports.download.subscribe(function (data) {
  var file = new Blob([data], {type: 'text/plain'});
  var anchor = document.createElement('a');
  anchor.href = URL.createObjectURL(file);
  anchor.download = 'data.txt';
  document.body.appendChild(anchor);
  anchor.click();
  document.body.removeChild(anchor);
});
```

## Поглиблений огляд

Elm, з огляду на шахрайські дії, уникає прямого запису на диск. Історично, цю можливість надавали низькорівневі мови. Але сучасні інтерактивні веб-додатки використовують джейсон, бази даних та API для обміну та зберігання даних. Як альтернативу, можна використати серверний код (Node.js, Python, тощо) для запису файлів.

## Див. також

- [Elm порти](https://guide.elm-lang.org/interop/ports.html)
- [File API на MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications)
- [Приклад створення і завантаження файлу в JavaScript](https://stackoverflow.com/questions/3665115/how-to-create-a-file-in-memory-for-user-to-download-but-not-through-server)
