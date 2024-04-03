---
date: 2024-01-20 18:01:42.164782-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) ."
lastmod: '2024-03-13T22:44:49.149795-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

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
