---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:55.075875-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Elm \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\
  \u043D\u043D\u043E\u0433\u043E \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0430\
  \ YAML, \u043F\u043E\u044D\u0442\u043E\u043C\u0443 \u043E\u0431\u044B\u0447\u043D\
  \u043E YAML \u043A\u043E\u043D\u0432\u0435\u0440\u0442\u0438\u0440\u0443\u044E\u0442\
  \ \u0432 JSON \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u0432\u043D\u0435\
  \u0448\u043D\u0435\u0433\u043E \u0438\u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\
  \u0442\u0430, \u0430 \u0437\u0430\u0442\u0435\u043C \u0440\u0430\u0431\u043E\u0442\
  \u0430\u044E\u0442 \u0441 \u043D\u0438\u043C \u0432 Elm\u2026"
lastmod: '2024-03-13T22:44:44.938555-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Elm \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\
  \u043D\u043E\u0433\u043E \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0430 YAML,\
  \ \u043F\u043E\u044D\u0442\u043E\u043C\u0443 \u043E\u0431\u044B\u0447\u043D\u043E\
  \ YAML \u043A\u043E\u043D\u0432\u0435\u0440\u0442\u0438\u0440\u0443\u044E\u0442\
  \ \u0432 JSON \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u0432\u043D\u0435\
  \u0448\u043D\u0435\u0433\u043E \u0438\u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\
  \u0442\u0430, \u0430 \u0437\u0430\u0442\u0435\u043C \u0440\u0430\u0431\u043E\u0442\
  \u0430\u044E\u0442 \u0441 \u043D\u0438\u043C \u0432 Elm \u0441 \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\u043C \u0431\u0438\u0431\
  \u043B\u0438\u043E\u0442\u0435\u043A\u0438 `elm/json`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

## Как это сделать:
В Elm нет встроенного парсинга YAML, поэтому обычно YAML конвертируют в JSON с помощью внешнего инструмента, а затем работают с ним в Elm с использованием библиотеки `elm/json`.

```elm
import Json.Decode exposing (Decoder, field, string, int, decodeValue)

type alias User =
    { name : String
    , age : Int
    }

userDecoder : Decoder User
userDecoder =
    Json.Decode.map2 User
        (field "name" string)
        (field "age" int)

jsonString : String
jsonString =
    """
    {
        "name": "Jane Doe",
        "age": 25
    }
    """

parseResult : Result String User
parseResult =
    jsonString
        |> Json.Decode.decodeString userDecoder

-- Пример вывода: Result.Ok { name = "Jane Doe", age = 25 }
```
Код Elm обрабатывает JSON, эквивалент вашего YAML после конвертации.

## Подробнее:
Простота YAML восходит к началу 2000-х как человекочитаемая альтернатива XML. Хотя Elm не анализирует YAML нативно, работа с JSON происходит легко, благодаря `elm/json`. Некоторые люди используют сторонние сервисы или инструменты, такие как `yaml-to-json.com` или даже пишут немного кода на стороне сервера на Node.js или Python, чтобы осуществить конвертацию из YAML в JSON. Помните, Elm отлично работает с JSON, поэтому этот двухэтапный процесс конвертации — это обходной путь, который обычно использует сообщество Elm.

## Смотрите также:
- Пакет Elm JSON: https://package.elm-lang.org/packages/elm/json/latest/
- Онлайн конвертер YAML в JSON: https://yaml-to-json.com/
- Генератор типов JSON в Elm: https://noredink.github.io/json-to-elm/
