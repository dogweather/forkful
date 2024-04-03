---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:57.296783-07:00
description: "\uBC29\uBC95: Elm\uC740 `Json.Decode` \uBC0F `Json.Encode` \uBAA8\uB4C8\
  \uC744 \uC8FC\uB85C \uC0AC\uC6A9\uD558\uC5EC JSON \uCC98\uB9AC\uB97C \uBA85\uC2DC\
  \uC801\uC774\uACE0 \uC548\uC804\uD558\uAC8C \uCC98\uB9AC\uD569\uB2C8\uB2E4. JSON\uC744\
  \ \uB2E4\uB8E8\uAE30 \uC2DC\uC791\uD558\uB824\uBA74, \uBA3C\uC800 \uB370\uC774\uD130\
  \ \uC720\uD615\uC5D0 \uB300\uD55C \uB514\uCF54\uB354\uB97C \uC815\uC758\uD574\uC57C\
  \ \uD569\uB2C8\uB2E4. \uC6B0\uB9AC\uAC00 \uAC04\uB2E8\uD55C \uC0AC\uC6A9\uC790 \uD504\
  \uB85C\uD544 \uAC1D\uCCB4\uB97C \uB2E4\uB8EC\uB2E4\uACE0 \uAC00\uC815\uD574\u2026"
lastmod: '2024-03-13T22:44:55.141359-06:00'
model: gpt-4-0125-preview
summary: "Elm\uC740 `Json.Decode` \uBC0F `Json.Encode` \uBAA8\uB4C8\uC744 \uC8FC\uB85C\
  \ \uC0AC\uC6A9\uD558\uC5EC JSON \uCC98\uB9AC\uB97C \uBA85\uC2DC\uC801\uC774\uACE0\
  \ \uC548\uC804\uD558\uAC8C \uCC98\uB9AC\uD569\uB2C8\uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

## 방법:
Elm은 `Json.Decode` 및 `Json.Encode` 모듈을 주로 사용하여 JSON 처리를 명시적이고 안전하게 처리합니다. JSON을 다루기 시작하려면, 먼저 데이터 유형에 대한 디코더를 정의해야 합니다. 우리가 간단한 사용자 프로필 객체를 다룬다고 가정해 봅시다.

먼저, Elm 유형을 정의하세요:

```elm
type alias UserProfile = 
    { id : Int
    , name : String
    , email : String
    }
```

### JSON을 Elm으로 디코딩
`UserProfile` 타입으로 JSON 문자열을 디코딩하기 위해, 디코더를 생성합니다:

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map3 UserProfile
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

JSON 객체를 디코딩하려면:

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decoded : Result String UserProfile
decoded =
    decodeString userProfileDecoder jsonString

{- 샘플 출력:
Result.Ok { id = 1, name = "John Doe", email = "john@example.com" }
-}
```

### Elm을 JSON으로 인코딩
Elm 값을 다시 JSON으로 인코딩하기 위해, `Json.Encode` 모듈을 활용합니다.

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
사용 예시:
encodeUserProfile { id = 1, name = "John Doe", email = "john@example.com" }

샘플 출력:
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### 타사 라이브러리
`elm-json-decode-pipeline`과 같은 Elm 패키지는 파이프라인 스타일을 사용하여 디코더를 생성하는 작업을 단순화할 수 있으며, 이는 복잡한 객체를 디코딩할 때 특히 유용합니다.

먼저, 라이브러리를 프로젝트에 추가하세요:

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

그런 다음, 디코더 정의를 다음과 같이 단순화할 수 있습니다:

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- 이 디코더를 이전과 같이 decodeString과 함께 사용하여 JSON 문자열을 디코딩하세요. -}
```

이 접근 방식은 디코더를 단순화하여 코드를 더 깨끗하고 유지보수하기 쉽게 만듭니다. 특히, 데이터 구조가 더 복잡해질수록 그렇습니다.
