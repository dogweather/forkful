---
title:                "Робота з JSON"
aliases:
- uk/elm/working-with-json.md
date:                  2024-02-03T19:23:08.004986-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Робота з JSON у Elm полягає в декодуванні даних JSON до типів Elm і кодуванні значень Elm назад у JSON. Цей процес є важливим для веб-додатків для взаємодії з API та зовнішніми джерелами даних, дозволяючи безперебійний обмін даними між клієнтом (Elm) та серверами або іншими сервісами.

## Як робити:

Elm ставиться до обробки JSON з явністю та безпечністю, в першу чергу використовуючи модулі `Json.Decode` та `Json.Encode`. Щоб почати працювати з JSON, спочатку вам потрібно визначити декодер для вашого типу даних. Припустимо, ми працюємо з простим об'єктом профілю користувача.

Спочатку визначте свій тип Elm:

```elm
type alias UserProfile = 
    { id : Int
    , name : String
    , email : String
    }
```

### Декодування JSON в Elm

Щоб декодувати рядок JSON в тип `UserProfile`, створіть декодер:

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map3 UserProfile
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

Для декодування об'єкта JSON:

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decoded : Result String UserProfile
decoded =
    decodeString userProfileDecoder jsonString

{- Приклад виводу:
Result.Ok { id = 1, name = "John Doe", email = "john@example.com" }
-}
```

### Кодування Elm у JSON

Щоб закодувати значення Elm назад у JSON, скористайтесь модулем `Json.Encode`.

```elm
import Json.Encode exposing (object, int, string)

encodeUserProfile : UserProfile -> String
encodeUserProfile userProfile =
    object
        [ ("id", int userProfile.id)
        , ("name", string userProfile.name)
        , ("email", string userProfile.email)
        ]
        |> Json.Encode.encode 0

{- 
Використання:
encodeUserProfile { id = 1, name = "John Doe", email = "john@example.com" }

Приклад виводу:
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### Сторонні бібліотеки

Пакети Elm, як-от `elm-json-decode-pipeline`, можуть спростити створення декодерів, використовуючи стиль конвеєра, що особливо зручно для декодування складних об'єктів.

Спочатку додайте бібліотеку до свого проекту:

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

Потім ви можете спростити визначення декодера так:

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- Використовуйте цей декодер як і раніше з decodeString для декодування рядків JSON. -}
```

Цей підхід спрощує декодер, роблячи код чистішим та легшим для підтримки, особливо коли структури даних стають складнішими.
