---
title:                "Elm: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜 HTTP 요청을 보내는가?

HTTP 요청은 인터넷에서 정보를 가져오는 일반적인 방법입니다. Elm에서 HTTP 요청을 보내는 것은 웹 프로그래밍에 필요한 기본적인 기술입니다. 이를 통해 다른 웹 사이트의 데이터를 가져와서 조작하거나 사용할 수 있습니다.

## 방법

```Elm
import Http
import Json.Decode exposing (Decoder, int, string, list)

type alias User = 
  { id : Int, 
    name : String, 
    interests : List String 
  }

-- 사용자의 데이터를 가져오기 위한 Json 디코딩 함수
userDecoder : Decoder User
userDecoder = 
  Decode.map3 User 
    (Decode.field "id" int) 
    (Decode.field "name" string)
    (Decode.field "interests" (Decode.list string))

-- 사용자 데이터를 가져오기 위한 HTTP 요청 함수
getUser : Http.Request User
getUser = 
  Http.get "https://samplewebsite.com/users/1" userDecoder

-- HTTP 요청을 보내고 데이터를 가져오는 함수
fetchUser : Cmd Msg
fetchUser = 
  Http.send UserFetched getUser

-- 사용자 데이터가 담긴 값을 저장하는 Msg 타입
type Msg = 
  UserFetched (Result Http.Error User)
```

위의 예제 코드에서는 Elm의 `Http` 모듈을 사용하여 HTTP 요청을 보내고, `Json.Decoder` 모듈을 사용하여 가져온 데이터를 디코딩하였습니다. 이제 `fetchUser` 함수를 호출하면 `UserFetched` 메시지가 생성되고, 해당 메시지를 통해 보낸 HTTP 요청의 결과를 확인할 수 있습니다.

## 깊이 있는 정보

HTTP 요청을 보내는 데는 여러 가지 방법이 있습니다. `Http` 모듈을 사용하는 것 외에도, `HttpBuilder` 모듈을 사용하거나, `Json.Decoder` 모듈 외에 다른 디코더를 사용할 수도 있습니다. 또한, 여러 개의 HTTP 요청을 보내고, 응답을 조합해서 사용할 수도 있습니다. 이러한 깊이 있는 개념을 이해하면 더 다양한 기능을 구현할 수 있습니다.

## 더보기

- Elm 공식 문서: https://guide.elm-lang.org/
- Elm 커뮤니티 블로그: https://www.elm-community.net/
- Elm 슬랙 채널: https://elmlang.herokuapp.com/