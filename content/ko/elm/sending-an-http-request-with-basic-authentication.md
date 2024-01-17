---
title:                "기본 인증으로 http 요청 보내기"
html_title:           "Elm: 기본 인증으로 http 요청 보내기"
simple_title:         "기본 인증으로 http 요청 보내기"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
Elm 프로그래머들은 기본 인증으로 HTTP 요청을 보내는 것이 무엇인지 알고 있어야 합니다. 이를 통해 API를 사용하거나 웹 서버와 통신하는 데 필요한 인증 절차를 처리할 수 있게 됩니다.

## 하는 법:
```Elm
import Http
import Json.Decode exposing (..)

url = "https://example.com/api"
username = "username"
password = "password"

request = Http.request 
    { method = "GET"
    , headers = [( "Authorization", "Basic " ++ username ++ ":" ++ password)]
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectString
    } 
```

## 깊이 파고들기:
Elm에서 기본 인증은 Http.request 함수를 통해 처리할 수 있습니다. 그러나 이 외에도 토큰 인증, OAuth 또는 OpenID Connect 같은 다른 인증 방식을 사용할 수도 있습니다.

## 더 많은 정보:
- [Elm으로 HTTP 요청 보내기](https://guide.elm-lang.org/effects/http.html)
- [HTTP 기본 인증](https://developer.mozilla.org/ko/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [HTTP 인증의 여러 유형](https://foodie.setOnline/basics/what-is-the-difference-between-http-basic-auth-vs-digest-auth/)