---
title:                "Работа с JSON"
aliases:
- ru/elm/working-with-json.md
date:                  2024-01-29T00:04:33.669891-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elm/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

JSON (JavaScript Object Notation) — это текстовый формат для обмена данными, аналогичный XML, но более легкий и удобочитаемый. Программисты на Elm используют JSON для отправки и получения данных на/с серверов, создавая динамичные, ориентированные на данные веб-приложения.

## Как это сделать:

Elm обрабатывает JSON с использованием модулей `Json.Decode` и `Json.Encode`. Вот простой пример:

```Elm
import Html exposing (text)
import Json.Decode exposing (string)

-- Декодирование простой JSON-строки
jsonString : String
jsonString = "{\"name\": \"Elm\"}"

type alias User =
    { name : String }

userNameDecoder : Json.Decode.Decoder String
userNameDecoder =
    Json.Decode.field "name" string

main =
    case Json.Decode.decodeString userNameDecoder jsonString of
        Ok name ->
            text ("Добро пожаловать, " ++ name)

        Err _ ->
            text "Упс, что-то пошло не так!"
```
Вывод: 
```
Добро пожаловать, Elm
```

## Глубокое погружение

JSON стал де-факто стандартом для веб-API с начала 2000-х годов, вытеснив XML своей простотой. Хотя Elm сжат и типобезопасен, обработка JSON может быть многословной из-за необходимости явных декодеров.

Альтернативы, такие как Haskell, используют типовые классы для кодирования/декодирования JSON, предоставляя больше функциональности «из коробки». Однако подход Elm помогает поддерживать типобезопасность и избегать ошибок времени выполнения. Декодеры явно указывают, как преобразовать JSON в типы Elm, а кодировщики делают обратный процесс.

## Смотрите также

Для дальнейшего чтения и ресурсов:

- Официальное руководство Elm по JSON: [Работа с JSON в Elm](https://guide.elm-lang.org/effects/json.html)
- Документация Json.Decode: [Elm Json.Decode](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
- Документация Json.Encode: [Elm Json.Encode](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode)
