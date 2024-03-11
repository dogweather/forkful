---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:33.669891-07:00
description: "JSON (JavaScript Object Notation) \u2014 \u044D\u0442\u043E \u0442\u0435\
  \u043A\u0441\u0442\u043E\u0432\u044B\u0439 \u0444\u043E\u0440\u043C\u0430\u0442\
  \ \u0434\u043B\u044F \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\
  \u044B\u043C\u0438, \u0430\u043D\u0430\u043B\u043E\u0433\u0438\u0447\u043D\u044B\
  \u0439 XML, \u043D\u043E \u0431\u043E\u043B\u0435\u0435 \u043B\u0435\u0433\u043A\
  \u0438\u0439 \u0438 \u0443\u0434\u043E\u0431\u043E\u0447\u0438\u0442\u0430\u0435\
  \u043C\u044B\u0439. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\
  \u0442\u044B \u043D\u0430 Elm\u2026"
lastmod: '2024-03-11T00:14:18.547163-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \u2014 \u044D\u0442\u043E \u0442\u0435\
  \u043A\u0441\u0442\u043E\u0432\u044B\u0439 \u0444\u043E\u0440\u043C\u0430\u0442\
  \ \u0434\u043B\u044F \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\
  \u044B\u043C\u0438, \u0430\u043D\u0430\u043B\u043E\u0433\u0438\u0447\u043D\u044B\
  \u0439 XML, \u043D\u043E \u0431\u043E\u043B\u0435\u0435 \u043B\u0435\u0433\u043A\
  \u0438\u0439 \u0438 \u0443\u0434\u043E\u0431\u043E\u0447\u0438\u0442\u0430\u0435\
  \u043C\u044B\u0439. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\
  \u0442\u044B \u043D\u0430 Elm\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
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
