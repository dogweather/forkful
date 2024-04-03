---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:33.669891-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Elm \u043E\u0431\u0440\u0430\u0431\u0430\u0442\u044B\u0432\u0430\u0435\
  \u0442 JSON \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\
  \u0438\u0435\u043C \u043C\u043E\u0434\u0443\u043B\u0435\u0439 `Json.Decode` \u0438\
  \ `Json.Encode`. \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u043F\
  \u0440\u0438\u043C\u0435\u0440."
lastmod: '2024-03-13T22:44:44.940130-06:00'
model: gpt-4-0125-preview
summary: "Elm \u043E\u0431\u0440\u0430\u0431\u0430\u0442\u044B\u0432\u0430\u0435\u0442\
  \ JSON \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\
  \u0438\u0435\u043C \u043C\u043E\u0434\u0443\u043B\u0435\u0439 `Json.Decode` \u0438\
  \ `Json.Encode`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

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
