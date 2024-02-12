---
title:                "Работа с YAML"
aliases:
- ru/elm/working-with-yaml.md
date:                  2024-01-29T00:04:55.075875-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elm/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
