---
title:                "JSON과 함께 일하기"
aliases:
- /ko/elm/working-with-json/
date:                  2024-02-03T19:22:57.296783-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON과 함께 일하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
Elm에서 JSON을 다루는 것은 JSON 데이터를 Elm 유형으로 디코딩하고, Elm 값들을 다시 JSON으로 인코딩하는 과정을 포함합니다. 이 과정은 웹 애플리케이션들이 API 및 외부 데이터 소스와 상호 작용하기 위해 필수적이며, 클라이언트(Elm)와 서버 혹은 다른 서비스들 간의 데이터 교환이 원활하게 이루어지게 합니다.

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
