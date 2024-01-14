---
title:                "Elm: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 기본 인증으로 보낼 이유는 시스템과의 상호작용을 위해 필요합니다.

## 하는 방법

"```Elm
import Http
import Basics
import Json.Encode as Json

type alias AuthHeader =
    { username: String, password: String }

type Msg
    = RequestFinished (Http.Result Http.Error String) 

sendAuthRequest : AuthHeader -> Cmd Msg
sendAuthRequest authHeader =
    let
        headers =
            [ ( "Authorization", "Basic " ++ Basics.toBase64 (authHeader.username ++ ":" ++ authHeader.password)  )
            , ( "Content-Type", "application/json" )
            ]

        request =
            Http.request
                { method = "POST"
                , headers = headers
                , url = "https://example.com/auth"
                , body = Http.emptyBody
                , timeout = Nothing
                , expect = Http.expectString (\response -> RequestFinished response)
                , onError = RequestFinished >> Result.Err
                }
    in
        request

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestFinished (Ok response) ->
            ( { model | authResponse = response }, Cmd.none )
            
        RequestFinished (Err error) ->
            ( { model | authResponse = "Error: " ++ (String.fromHttpError error) }, Cmd.none )
            
        _ ->
            ( model, Cmd.none )
            
main : Program Never Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

"```

위 코드는 인증 헤더를 포함한 기본 인증 요청을 보내는 방법을 보여줍니다. 인증을 위해 필요한 헤더를 설정하고, Http.request를 사용하여 POST 요청을 보냅니다. 결과는 RequestFinished 메시지로 처리되며 성공적인 경우 response는 String으로, 실패한 경우 Http.Error가 반환됩니다.

## 깊이 파고들기

HTTP를 사용하여 보안을 강화하기 위해 기본 인증을 사용할 때, Authorization 헤더의 값은 "Basic"으로 시작하고 이어서 사용자 이름과 비밀번호를 base64로 인코딩한 문자열이 추가로 붙어서 전송됩니다. 각 언어별로 base64 인코딩 방법이 다르므로 주의해야 합니다.

## 더 알아보기

- [Elm 공식 문서](https://elm-lang.org/docs)
- [HTTP 모듈 문서](https://package.elm-lang.org/packages/elm-lang/http/latest/)
- [Base64 인코딩 관련 자료](https://gist.github.com/stephen-soltesz/669ae761fad8433b048c)
- [HTTP 기본 인증 관련 문서](https://datatracker.ietf.org/doc/html/rfc7617)