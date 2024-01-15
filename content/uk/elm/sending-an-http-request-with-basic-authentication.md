---
title:                "Надсилання запиту http з базовою автентифікацією"
html_title:           "Elm: Надсилання запиту http з базовою автентифікацією"
simple_title:         "Надсилання запиту http з базовою автентифікацією"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Відправка HTTP-запиту з базовою аутентифікацією є важливим етапом для багатьох програм, які використовують зовнішні ресурси або міждоменні комунікації. Це забезпечує безпечний доступ до даних та інтерактивацію зі сторонніми сервісами.

## Як

```elm
import Http exposing (..)
import Json.Decode as Decode

-- Створення запиту з базовою аутентифікацією
basicAuthRequest : String -> String -> String -> String -> String -> String -> Http.RequestBody -> Http.Request
basicAuthRequest method baseUrl username password url body =
    let
        headers =
            [ ("Authorization", "Basic " ++ Base64.encode (username ++ ":" ++ password))
            ]
    in
    Http.request
        { method = method
        , headers = headers
        , url = baseUrl ++ url
        , body = body
        , expect = expectJsonResponse decoder
        , timeout = Nothing
        , withCredentials = False
        }

-- Декодування відповіді JSON
type alias ResponseBody =
    { name : String
    , id : Int
    }

decoder : Decode.Decoder ResponseBody
decoder =
    Decode.succeed ResponseBody
        |> Decode.field "name" Decode.string
        |> Decode.field "id" Decode.int

-- Відправка запиту
sendRequest : Cmd Msg
sendRequest =
    Http.send handleResponse <| basicAuthRequest "POST" "https://example.com/" "username" "password" "api/endpoint" "{}"

-- Обробка відповіді
handleResponse : Result Http.Error ResponseBody -> Msg
handleResponse result =
    case result of
        Ok response ->
            Msg.Success response

        Err error ->
            Msg.Failure error
```

Приклад вхідної та вихідної даних:

Вхідні дані:
```json
{
    "name": "John",
    "id": 123
}
```

Вихідні дані:
```elm
ResponseBody "John" 123
```

## Deep Dive

Відправка HTTP-запиту з базовою аутентифікацією використовує заголовок "Authorization", який містить Base64-кодовані дані з іменем користувача та паролем. Така аутентифікація забезпечує задану рівню безпеки для виклику зовнішніх ресурсів. Також важливим етапом є правильне декодування отриманої відповіді, щоб можна було коректно обробити дані.

## Див. також

- [Документація Elm по відправленню HTTP-запитів](https://package.elm-lang.org/packages/elm/http/latest/Http)
- [Документація Elm по базовій аутентифікації](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Приклад Elm-додатка, що відправляє HTTP-запити з базовою аутентифікацією](https://github.com/example/elm-http-basic-auth-example)