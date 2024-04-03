---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:17.906699-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Elm \u0438\u0437\u043D\u0430\u0447\u0430\u043B\u044C\u043D\u043E \u043D\
  \u0435 \u0430\u043D\u0430\u043B\u0438\u0437\u0438\u0440\u0443\u0435\u0442 \u0441\
  \u044B\u0440\u043E\u0439 HTML \u0441\u0430\u043C\u043E\u0441\u0442\u043E\u044F\u0442\
  \u0435\u043B\u044C\u043D\u043E; \u0432\u043C\u0435\u0441\u0442\u043E \u044D\u0442\
  \u043E\u0433\u043E, \u043E\u043D \u0441\u043E\u0441\u0440\u0435\u0434\u043E\u0442\
  \u0430\u0447\u0438\u0432\u0430\u0435\u0442\u0441\u044F \u043D\u0430 \u043E\u0442\
  \u0440\u0438\u0441\u043E\u0432\u043A\u0435 \u043F\u0440\u0435\u0434\u0441\u0442\u0430\
  \u0432\u043B\u0435\u043D\u0438\u0439 \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\
  \u044E \u0441\u0432\u043E\u0435\u0433\u043E\u2026"
lastmod: '2024-03-13T22:44:44.897174-06:00'
model: gpt-4-0125-preview
summary: "Elm \u0438\u0437\u043D\u0430\u0447\u0430\u043B\u044C\u043D\u043E \u043D\u0435\
  \ \u0430\u043D\u0430\u043B\u0438\u0437\u0438\u0440\u0443\u0435\u0442 \u0441\u044B\
  \u0440\u043E\u0439 HTML \u0441\u0430\u043C\u043E\u0441\u0442\u043E\u044F\u0442\u0435\
  \u043B\u044C\u043D\u043E; \u0432\u043C\u0435\u0441\u0442\u043E \u044D\u0442\u043E\
  \u0433\u043E, \u043E\u043D \u0441\u043E\u0441\u0440\u0435\u0434\u043E\u0442\u0430\
  \u0447\u0438\u0432\u0430\u0435\u0442\u0441\u044F \u043D\u0430 \u043E\u0442\u0440\
  \u0438\u0441\u043E\u0432\u043A\u0435 \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\
  \u043B\u0435\u043D\u0438\u0439 \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E\
  \ \u0441\u0432\u043E\u0435\u0433\u043E \u043C\u043E\u0434\u0443\u043B\u044F `Html`."
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
weight: 43
---

## Как это сделать:
Elm изначально не анализирует сырой HTML самостоятельно; вместо этого, он сосредотачивается на отрисовке представлений с помощью своего модуля `Html`. Для анализа HTML обычно используют серверный сервис или внешнюю JavaScript библиотеку, затем передают данные в Elm. Вот базовая настройка Elm для работы с проанализированными данными HTML:

```Elm
module Main exposing (main)

import Html exposing (Html, text)
import Json.Decode as Decode

type alias HtmlData =
    { tag : String
    , attributes : List (String, String)
    , content : String
    }

-- Предполагая, что у вас есть объект JSON, который представляет ваши данные HTML
htmlDataDecoder : Decode.Decoder HtmlData
htmlDataDecoder =
    Decode.map3 HtmlData
        (Decode.field "tag" Decode.string)
        (Decode.field "attributes" (Decode.list (Decode.tuple Decode.string Decode.string)))
        (Decode.field "content" Decode.string)

-- Замените на фактический JSON с сервера или внешней библиотеки JS
sampleJson : String
sampleJson = 
    """
    {"tag":"div", "attributes": [["class", "container"]], "content": "Привет, Elm!"}
    """

-- Декодирование образца JSON в HtmlData
decodedHtmlData : Result Decode.Error HtmlData
decodedHtmlData =
    Decode.decodeString htmlDataDecoder sampleJson

-- Отображение представления из наших данных HtmlData
view : HtmlData -> Html msg
view htmlData =
    Html.text (htmlData.content)

main : Html msg
main =
    case decodedHtmlData of
        Ok data ->
            view data
        
        Err error ->
            text "Не удалось проанализировать данные HTML"

```

Эта простая настройка показывает, как вы начнете работать с проанализированными данными HTML в Elm.

## Подробнее
Исторически, сильное акцентирование Elm на типовой безопасности и надежной архитектуре означает, что прямой анализ HTML не является его сильной стороной. Elm блестит в создании надежных веб-приложений с минимальным количеством ошибок во время выполнения.

Для анализа HTML обычно используется JavaScript, который имеет зрелые библиотеки, такие как `DOMParser` и `$.parseHTML` от jQuery. Всю тяжелую работу вы выполняете в JavaScript, затем отправляете дерево разбора в Elm как сериализованный JSON. Для этого вы можете использовать порты (способ взаимодействия Elm с JavaScript) или веб-сервисы.

После попадания в Elm, декодеры JSON превращают сериализованные данные в структурированные типы Elm. При подходе декодирования JSON вы наслаждаетесь типовой безопасностью Elm и можете держать грязную логику анализа HTML вне вашего кодовой базы Elm.

Альтернативы? Если вам абсолютно необходимо анализировать HTML внутри Elm, вероятно, вам потребуется индивидуальное решение. Это может включать использование серверного парсера, который предоставляет API или пакет Elm, если вы найдете подходящий, хотя на данный момент выбор ограничен.

## Смотрите также
Для получения дополнительной информации о декодировании JSON в Elm:
- Официальное руководство по Elm о JSON: https://guide.elm-lang.org/effects/json.html

Порты Elm для взаимодействия с JavaScript:
- Руководство Elm по портам: https://guide.elm-lang.org/interop/ports.html

Обсуждения и познания сообщества:
- Elm Discourse: https://discourse.elm-lang.org/
- Канал Elm в Slack, где вы можете попросить о помощи и обсудить: https://elmlang.herokuapp.com/
