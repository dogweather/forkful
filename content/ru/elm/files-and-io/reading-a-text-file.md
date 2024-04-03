---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:15.834670-07:00
description: "\u041A\u0430\u043A: Elm \u0432 \u043E\u0441\u043D\u043E\u0432\u043D\u043E\
  \u043C \u0441\u043E\u0441\u0440\u0435\u0434\u043E\u0442\u043E\u0447\u0435\u043D\
  \ \u043D\u0430 \u0440\u0430\u0437\u0440\u0430\u0431\u043E\u0442\u043A\u0435 \u0434\
  \u043B\u044F \u0444\u0440\u043E\u043D\u0442\u0435\u043D\u0434\u0430, \u0433\u0434\
  \u0435 \u043F\u0440\u044F\u043C\u043E\u0439 \u0434\u043E\u0441\u0442\u0443\u043F\
  \ \u043A \u0444\u0430\u0439\u043B\u043E\u0432\u043E\u0439 \u0441\u0438\u0441\u0442\
  \u0435\u043C\u0435 \u043D\u0435\u0432\u043E\u0437\u043C\u043E\u0436\u0435\u043D\
  \ \u043F\u043E \u0441\u043E\u043E\u0431\u0440\u0430\u0436\u0435\u043D\u0438\u044F\
  \u043C \u0431\u0435\u0437\u043E\u043F\u0430\u0441\u043D\u043E\u0441\u0442\u0438\
  . \u0412\u043C\u0435\u0441\u0442\u043E \u044D\u0442\u043E\u0433\u043E\u2026"
lastmod: '2024-03-13T22:44:44.933640-06:00'
model: gpt-4-0125-preview
summary: "Elm \u0432 \u043E\u0441\u043D\u043E\u0432\u043D\u043E\u043C \u0441\u043E\
  \u0441\u0440\u0435\u0434\u043E\u0442\u043E\u0447\u0435\u043D \u043D\u0430 \u0440\
  \u0430\u0437\u0440\u0430\u0431\u043E\u0442\u043A\u0435 \u0434\u043B\u044F \u0444\
  \u0440\u043E\u043D\u0442\u0435\u043D\u0434\u0430, \u0433\u0434\u0435 \u043F\u0440\
  \u044F\u043C\u043E\u0439 \u0434\u043E\u0441\u0442\u0443\u043F \u043A \u0444\u0430\
  \u0439\u043B\u043E\u0432\u043E\u0439 \u0441\u0438\u0441\u0442\u0435\u043C\u0435\
  \ \u043D\u0435\u0432\u043E\u0437\u043C\u043E\u0436\u0435\u043D \u043F\u043E \u0441\
  \u043E\u043E\u0431\u0440\u0430\u0436\u0435\u043D\u0438\u044F\u043C \u0431\u0435\u0437\
  \u043E\u043F\u0430\u0441\u043D\u043E\u0441\u0442\u0438."
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 22
---

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
