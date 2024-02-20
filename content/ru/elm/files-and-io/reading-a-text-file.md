---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:15.834670-07:00
description: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 - \u044D\u0442\u043E \u0438\
  \u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u0441\u043E\u0434\u0435\u0440\
  \u0436\u0438\u043C\u043E\u0433\u043E \u0438\u0437 \u0444\u0430\u0439\u043B\u0430\
  , \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u0441\u0442\u0440\u0443\u043A\u0442\
  \u0443\u0440\u0438\u0440\u043E\u0432\u0430\u043D \u043A\u0430\u043A \u0447\u0438\
  \u0442\u0430\u0435\u043C\u044B\u0439 \u0442\u0435\u043A\u0441\u0442, \u0430 \u043D\
  \u0435 \u0434\u0432\u043E\u0438\u0447\u043D\u044B\u0435 \u0434\u0430\u043D\u043D\
  \u044B\u0435. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\
  \u044B \u0447\u0438\u0442\u0430\u044E\u0442\u2026"
lastmod: 2024-02-19 22:05:03.883118
model: gpt-4-0125-preview
summary: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430 - \u044D\u0442\u043E \u0438\
  \u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u0441\u043E\u0434\u0435\u0440\
  \u0436\u0438\u043C\u043E\u0433\u043E \u0438\u0437 \u0444\u0430\u0439\u043B\u0430\
  , \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u0441\u0442\u0440\u0443\u043A\u0442\
  \u0443\u0440\u0438\u0440\u043E\u0432\u0430\u043D \u043A\u0430\u043A \u0447\u0438\
  \u0442\u0430\u0435\u043C\u044B\u0439 \u0442\u0435\u043A\u0441\u0442, \u0430 \u043D\
  \u0435 \u0434\u0432\u043E\u0438\u0447\u043D\u044B\u0435 \u0434\u0430\u043D\u043D\
  \u044B\u0435. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\
  \u044B \u0447\u0438\u0442\u0430\u044E\u0442\u2026"
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
---

{{< edit_this_page >}}

## Что и Почему?

Чтение текстового файла - это извлечение содержимого из файла, который структурирован как читаемый текст, а не двоичные данные. Программисты читают текстовые файлы для доступа к данным, конфигурациям или для импорта большого количества текста в свои приложения.

## Как:

Elm в основном сосредоточен на разработке для фронтенда, где прямой доступ к файловой системе невозможен по соображениям безопасности. Вместо этого вы обрабатываете загрузки файлов пользователями. Вот как вы можете прочитать текстовый файл, выбранный пользователем:

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
        [ button [ onClick SelectFile ] [ text "Выбрать текстовый файл" ]
        , div [] [ text model.fileContent ]
        ]

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
```

Запустите код в браузере, нажмите кнопку и выберите текстовый файл. Он отобразит содержимое в вашем приложении Elm.

## Глубокое погружение

Elm не читает файлы напрямую с файловой системы сервера - он не был разработан для серверных операций. Вместо этого Elm управляет вводом файлов через File API в браузере, обычно инициируемый действием пользователя, таким как выбор файла или действие перетаскивания. Это мера безопасности.

В прошлом вы могли использовать JavaScript и Node.js для чтения файлов на стороне сервера или XMLHttpRequest (XHR) для чтения на стороне клиента без взаимодействия пользователя. У них разные модели безопасности и возможности.

Модули `File` и `File.Selector` в Elm делают работу с чтением файлов в браузере довольно гладкой, но помните о философии Elm "нет побочных эффектов". Это означает, что чтение файлов строго контролируется, требуется явное действие пользователя. Также разбор и декодирование содержимого файла требует внимания, чтобы соответствовать строгой типизации Elm.

## Смотрите также

- Официальная документация Elm File API: https://package.elm-lang.org/packages/elm/file/latest/
- Руководство по командам и подпискам Elm (для понимания асинхронных операций): https://guide.elm-lang.org/effects/
- Elm Discuss для вопросов и общения с сообществом: https://discourse.elm-lang.org/
