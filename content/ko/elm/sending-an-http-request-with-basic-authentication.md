---
title:                "기본 인증을 이용한 HTTP 요청 보내기"
html_title:           "Arduino: 기본 인증을 이용한 HTTP 요청 보내기"
simple_title:         "기본 인증을 이용한 HTTP 요청 보내기"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 그렇게 하는가?
HTTP 기본 인증을 통한 요청은 서버와 클라이언트 간의 인증을 단순하게 구현할 수 있는 방법입니다. 이 방식은 특히 API 호출에서 익명 사용자의 접근을 제한하거나 특정 권한을 가진 사용자만 API를 호출하도록 제한할 때 사용됩니다.

## 어떻게 할까?
다음은 Elm에서 HTTP 요청을 보내는 기본적인 예제입니다.
```Elm
import Http
import Json.Decode as Decoder
import Base64

basicAuth : String -> String -> Http.Header
basicAuth username password =
    let
        userpass = username ++ ":" ++ password
        encoded = Base64.encode userpass
    in
    Http.header "Authorization" ("Basic " ++ encoded)

fetch : Http.Request String
fetch =
    Http.request
        { method = "GET"
        , headers = [ basicAuth "myUsername" "myPassword" ]
        , url = "http://my-api-endpoint.com"
        , body = Http.emptyBody
        , expect = Http.expectString Decoder.identity
        , timeout = Nothing
        , tracker = Nothing
        }

main =
    fetch
    |> Http.send HandleResponse
```

## 깊은 이해
HTTP 기본 인증이 처음 등장한 것은 1996년 RFC 1945에서였으며 당시 널리 사용되는 방식 중 하나였습니다. 하지만 이 방식은 SSL/TLS와 함께 사용해야 메시지가 허용되지 않는 자에게 노출되는 것을 방지할 수 있습니다. 대안적으로 OAuth, JWT 등의 보다 복잡하지만 보안성이 높은 인증 방식이 있습니다.

## 참고 자료
더 많은 정보를 원한다면 다음 링크를 참조하세요:
1. Elm의 Http 모듈 문서: https://package.elm-lang.org/packages/elm/http/latest/
2. Base64 인코딩에 대한 정보: https://en.wikipedia.org/wiki/Base64
3. HTTP 인증: https://developer.mozilla.org/ko/docs/Web/HTTP/Authentication 
4. Elm에서의 API 호출에 관한 훌륭한 자료: https://korban.net/posts/elm/2018-11-28-Practical-guide-fetching-data-in-Elm-part-1/