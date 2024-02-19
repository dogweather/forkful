---
aliases:
- /uk/elm/writing-a-text-file/
date: 2024-01-19
description: "\u0417\u0430\u043F\u0438\u0441 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443 \u2014 \u0446\u0435 \u043F\u0440\
  \u043E\u0446\u0435\u0441 \u0437\u0431\u0435\u0440\u0435\u0436\u0435\u043D\u043D\u044F\
  \ \u0434\u0430\u043D\u0438\u0445 \u0443 \u0444\u0430\u0439\u043B \u043D\u0430 \u0434\
  \u0438\u0441\u043A\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\
  \u0442\u0438 \u0446\u0435 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0434\u043B\
  \u044F \u0437\u0431\u0435\u0440\u0435\u0436\u0435\u043D\u043D\u044F \u0440\u0435\
  \u0437\u0443\u043B\u044C\u0442\u0430\u0442\u0456\u0432, \u043A\u043E\u043D\u0444\
  \u0456\u0433\u0443\u0440\u0430\u0446\u0456\u0439, \u0430\u0431\u043E \u043E\u0431\
  \u043C\u0456\u043D\u0443 \u0434\u0430\u043D\u0438\u043C\u0438."
lastmod: 2024-02-18 23:09:00.224493
summary: "\u0417\u0430\u043F\u0438\u0441 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443 \u2014 \u0446\u0435 \u043F\u0440\
  \u043E\u0446\u0435\u0441 \u0437\u0431\u0435\u0440\u0435\u0436\u0435\u043D\u043D\u044F\
  \ \u0434\u0430\u043D\u0438\u0445 \u0443 \u0444\u0430\u0439\u043B \u043D\u0430 \u0434\
  \u0438\u0441\u043A\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\
  \u0442\u0438 \u0446\u0435 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0434\u043B\
  \u044F \u0437\u0431\u0435\u0440\u0435\u0436\u0435\u043D\u043D\u044F \u0440\u0435\
  \u0437\u0443\u043B\u044C\u0442\u0430\u0442\u0456\u0432, \u043A\u043E\u043D\u0444\
  \u0456\u0433\u0443\u0440\u0430\u0446\u0456\u0439, \u0430\u0431\u043E \u043E\u0431\
  \u043C\u0456\u043D\u0443 \u0434\u0430\u043D\u0438\u043C\u0438."
title: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
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
