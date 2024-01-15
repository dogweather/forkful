---
title:                "HTTP 요청 보내기"
html_title:           "Elm: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜
HTTP 요청을 보내는 것에 대해 궁금한 독자들이 많아서 이것에 대해 간단한 설명을 해보고자 합니다. Elm에서 HTTP 요청은 어떻게 작동하는지 차근차근 알아보도록 하겠습니다.

## 어떻게
```Elm
import Http
import Json.Decode exposing (int, string)

type Msg
    = FetchSuccess (Http.Response (Result Http.Error
Something))
    | FetchFailed Http.Error

type Thing = Thing
    { id : Int
    , name : String
    }

stringDecoder : Json.Decode.Decoder Thing
stringDecoder =
    Json.Decode.map3 Thing
        (Json.Decode.field "id" int)
        (Json.Decode.field "name" string)

intDecoder : Json.Decode.Decoder Int
intDecoder =
    Json.Decode.field "id" int

[ Http.send FetchSuccess
    (Http.get "/api/thing/1" intDecoder)
, Http.send FetchFailed
    (Http.delete "/api/thing/5" stringDecoder)
]
```

위의 예제 코드는 Elm에서 HTTP 요청을 보내는 방법을 보여주고 있습니다. 먼저, Http 모듈을 import하고 Json.Decode 모듈에서 int와 string decoder를 이용하여 데이터를 decoding합니다. 그리고, Msg 타입을 정의하여 요청이 성공하거나 실패한 경우에 대한 처리를 할 수 있도록 합니다. 이제, Http.get과 Http.delete를 이용하여 서버로 요청을 보내고, 결과를 Msg에 맞게 처리합니다.

## 더 깊이
HTTP 요청을 보내기 위해서는 Elm에서 제공하는 Http 모듈을 이용해야 합니다. 이 모듈을 사용하면 GET, POST, PUT, DELETE 등 다양한 HTTP 메소드를 이용할 수 있습니다. 또한, Json.Decode 모듈을 이용하여 받아온 데이터를 decoding할 수도 있습니다. 더 자세한 정보는 [Elm 공식 문서](https://package.elm-lang.org/packages/elm/http/latest/)를 참고하시기 바랍니다.

## 더 읽어보기
- [Elm: HTTP](https://guide.elm-lang.org/effects/http.html)
- [Creating and using Elm HTTP requests with Types](https://medium.com/@ckoster22/creating-and-using-elm-http-requests-with-types-bb3d0c592780)
- [HTTP Requests and Elm](https://thoughtbot.com/blog/http-requests-and-elm)