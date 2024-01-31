---
title:                "JSON 다루기"
date:                  2024-01-19
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 쓰나요?)
JSON은 자바스크립트 기반 데이터 형식입니다. 프로그래머들은 웹과 서버 간 정보를 쉽게 주고받기 위해 이를 사용합니다.

## How to: (어떻게 사용하나요?)
Elm에서 JSON을 다루기 위해, `Json.Decode`와 `Json.Encode` 모듈을 사용합니다. 아래 예시는 간단한 JSON 오브젝트를 디코드하는 방법을 보여줍니다.

```Elm
import Json.Decode exposing (Decoder, decodeString, field, string)

type alias User =
    { name : String
    , age : String
    }

userDecoder : Decoder User
userDecoder =
    field "name" string
        |> Json.Decode.andThen (\name ->
            field "age" string
                |> Json.Decode.map (User name)
           )

jsonString : String
jsonString =
    """{"name": "홍길동", "age": "30"}"""

decodeResult : Result String User
decodeResult =
    decodeString userDecoder jsonString

-- decodeResult는 Result Ok { name = "홍길동", age = "30" }입니다.
```

## Deep Dive (깊이 알아보기)
JSON은 JavaScript Object Notation의 줄임말로, 2001년에 도입되었습니다. Elm에서는 처음부터 불변성과 타입 안전성을 제공합니다. `Json.Decode`와 `Json.Encode` 모듈은 이러한 특징을 반영하여 타입 오류를 방지합니다. JSON을 디코드할 때, 매핑 함수를 사용하여 Elm의 타입 안전한 구조로 변환합니다.

## See Also (더 알아보기)
- Elm 공식 문서의 JSON 가이드: https://guide.elm-lang.org/interop/json.html
- `Json.Decode` API 문서: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode
- `Json.Encode` API 문서: https://package.elm-lang.org/packages/elm/json/latest/Json-Encode
