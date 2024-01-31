---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
date:                  2024-01-20T18:01:42.164782-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"

category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
HTTP запит з базовою аутентифікацією - це спосіб використання логіна і пароля для доступу до ресурсів сервера. Програмісти роблять це для захисту важливих даних та контролю доступу до API.

## How to: (Як це зробити:)
```Elm
import Http
import Base64 exposing (encode)

type alias Model =
    { username : String
    , password : String
    }

model : Model
model =
    { username = "user"
    , password = "pass"
    }

type Msg
    = GotData (Result Http.Error String)

basicAuthHeader : Model -> Http.Header
basicAuthHeader creds =
    let
        encodedCredentials =
            encode (creds.username ++ ":" ++ creds.password)
    in
    Http.header "Authorization" ("Basic " ++ encodedCredentials)

requestData : Model -> Cmd Msg
requestData creds =
    Http.get
        { url = "https://your.api/endpoint"
        , headers = [ basicAuthHeader creds ]
        }
        |> Http.expectString GotData
```

Цей код ініціалізує запит HttpMethod.Get з базовими аутентифікаційними даними. Якщо сервер підтверджує ці дані, він повертає відповідний вміст.

## Deep Dive (Поглиблений Аналіз):
HTTP базова аутентифікація - історично одна з перших метод поданий у 1996 (RFC 1945) для захисту веб-ресурсів. Сьогодні існують безпечніші методи, наприклад OAuth 2.0 чи JWT, але базова аутентифікація ефективна для простих API або закритих систем. Elm використовує модуль `Http` для створення запитів і встановлення заголовків, таких як `Authorization`. Безпечно передавайте і зберігайте аутентифікаційні дані.

## See Also (Дивіться Також):
- Elm HTTP package: [http://package.elm-lang.org/packages/elm/http/latest](http://package.elm-lang.org/packages/elm/http/latest)
- HTTP Authentication: Basic and Digest Access Authentication: [https://tools.ietf.org/html/rfc2617](https://tools.ietf.org/html/rfc2617)
- Elm Base64 package: [http://package.elm-lang.org/packages/truqu/elm-base64/latest](http://package.elm-lang.org/packages/truqu/elm-base64/latest)
