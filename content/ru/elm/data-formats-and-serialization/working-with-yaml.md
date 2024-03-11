---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:55.075875-07:00
description: "YAML \u2014 \u044D\u0442\u043E \u0443\u0434\u043E\u0431\u043D\u044B\u0439\
  \ \u0434\u043B\u044F \u0432\u043E\u0441\u043F\u0440\u0438\u044F\u0442\u0438\u044F\
  \ \u0447\u0435\u043B\u043E\u0432\u0435\u043A\u043E\u043C \u0441\u0442\u0430\u043D\
  \u0434\u0430\u0440\u0442 \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\
  \u0438\u0438 \u0434\u0430\u043D\u043D\u044B\u0445, \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u0443\u0435\u043C\u044B\u0439 \u0434\u043B\u044F \u0444\u0430\u0439\
  \u043B\u043E\u0432 \u043A\u043E\u043D\u0444\u0438\u0433\u0443\u0440\u0430\u0446\u0438\
  \u0438 \u0438 \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\u044B\
  \u043C\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\
  \u0430\u043C \u043E\u043D \u043D\u0440\u0430\u0432\u0438\u0442\u0441\u044F\u2026"
lastmod: '2024-03-11T00:14:18.545550-06:00'
model: gpt-4-0125-preview
summary: "YAML \u2014 \u044D\u0442\u043E \u0443\u0434\u043E\u0431\u043D\u044B\u0439\
  \ \u0434\u043B\u044F \u0432\u043E\u0441\u043F\u0440\u0438\u044F\u0442\u0438\u044F\
  \ \u0447\u0435\u043B\u043E\u0432\u0435\u043A\u043E\u043C \u0441\u0442\u0430\u043D\
  \u0434\u0430\u0440\u0442 \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\
  \u0438\u0438 \u0434\u0430\u043D\u043D\u044B\u0445, \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u0443\u0435\u043C\u044B\u0439 \u0434\u043B\u044F \u0444\u0430\u0439\
  \u043B\u043E\u0432 \u043A\u043E\u043D\u0444\u0438\u0433\u0443\u0440\u0430\u0446\u0438\
  \u0438 \u0438 \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\u044B\
  \u043C\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\
  \u0430\u043C \u043E\u043D \u043D\u0440\u0430\u0432\u0438\u0442\u0441\u044F\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
---

{{< edit_this_page >}}

## Что и Почему?

YAML — это удобный для восприятия человеком стандарт сериализации данных, используемый для файлов конфигурации и обмена данными. Программистам он нравится потому, что он ясный, легко читаемый и широко принят в инструментах и языках.

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
