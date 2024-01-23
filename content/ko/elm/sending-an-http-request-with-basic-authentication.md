---
title:                "기본 인증을 사용한 HTTP 요청 보내기"
date:                  2024-01-20T18:01:28.426958-07:00
model:                 gpt-4-1106-preview
simple_title:         "기본 인증을 사용한 HTTP 요청 보내기"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTTP 요청은 데이터를 주고받을 때 씁니다. 기본 인증(basic authentication)은 사용자 이름과 비밀번호를 인코딩해 서버로 보내 안전하게 로그인합니다.

## How to: (어떻게:)
Elm에서 기본 인증을 사용해 HTTP 요청을 보내려면 `Http` 모듈을 사용합니다. 아래는 기본 인증으로 `GET` 요청을 보내는 예제 코드입니다.

```Elm
import Http
import Base64

type Msg
    = GotData (Result Http.Error String)

basicAuth : String -> String -> List Http.Header
basicAuth username password =
    let
        credentials = username ++ ":" ++ password
        encodedCredentials = Base64.encode credentials
    in
    [ Http.header "Authorization" ("Basic " ++ encodedCredentials) ]

getProtectedResource : Cmd Msg
getProtectedResource =
    Http.get
        { url = "https://yourapi.com/protected/resource"
        , headers = basicAuth "your_username" "your_password"
        , expect = Http.expectString GotData
        }

-- 예제 출력을 위한 update 함수
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotData (Ok data) ->
            -- 성공한 경우 데이터를 처리
            ( { model | content = data }, Cmd.none )

        GotData (Err error) ->
            -- 오류 처리
            ( { model | content = "Failed to fetch data: " ++ Debug.toString error }, Cmd.none )
```

## Deep Dive (심층 탐구)
- Elm에서 HTTP 요청을 할 때 `Http` 모듈을 사용합니다.
- 기본 인증은 `Authorization` 헤더에 "Basic" 다음에 Base64로 인코딩된 사용자 이름과 비밀번호를 붙여 전송합니다.
- Base64 인코딩은 Elm에서 `Base64` 모듈을 사용하여 처리할 수 있습니다.
- Elm 0.19 이상에서는 `elm/http` 패키지가 필요합니다. 업데이트로 인해 `elm/http`가 많이 개선되었습니다.
- 선택적으로, 더 안전한 인증을 위해 `Http` 요청과 함께 OAuth나 토큰 기반 인증을 사용할 수도 있습니다.
- 기본 인증 방식은 HTTPS와 함께 사용할 때 안전합니다. 그러나 보안이 중요한 경우 더 나은 방법을 고려해야 합니다.

## See Also (참고 자료)
- Elm HTTP 패키지: [package.elm-lang.org/packages/elm/http/latest](https://package.elm-lang.org/packages/elm/http/latest)
- Elm Base64 모듈: [package.elm-lang.org/packages/truqu/elm-base64/latest](https://package.elm-lang.org/packages/truqu/elm-base64/latest)
- HTTP 기본 인증에 대한 설명: [developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- Elm에서 안전한 HTTP 요청을 보내는 방법: [elmprogramming.com](https://elmprogramming.com/) (안전한 통신 부분 참고)
