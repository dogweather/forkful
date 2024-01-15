---
title:                "json 작업하기"
html_title:           "Elm: json 작업하기"
simple_title:         "json 작업하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/working-with-json.md"
---

{{< edit_this_page >}}

## 왜
JSON은 Elm에서 가장 일반적으로 사용되는 데이터 형식 중 하나입니다. JSON을 이해하고 다루는 방법을 익히면 Elm 프로그램을 작성하고 데이터를 다룰 때 매우 유용하게 활용할 수 있습니다.

## 어떻게
JSON은 JavaScript에서 사용되는 형식이므로 Elm 코드에서도 매우 간단하게 다룰 수 있습니다. 다음 예제를 통해 JSON 데이터를 다루는 간단한 방법을 살펴보겠습니다.

```Elm
import Http
import Json.Decode as Decode
import Json.Encode as Encode

-- JSON 데이터를 받아와서 파싱하기
type alias User =
    { name : String
    , age : Int
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

getUser : Http.Request User
getUser =
    Http.get
        { url = "https://example.com/api/user"
        , expect = Http.expectJson userDecoder
        }

-- JSON 데이터를 생성하기
user : User
user =
    { name = "John"
    , age = 25
    }

jsonString : String
jsonString =
    Encode.encode 0 user
```
파싱된 데이터를 사용하는 방법은 일반적인 Elm 데이터를 사용하는 것과 같습니다.

## 딥 다이브
더 복잡한 JSON 객체를 다룰 때는 `map2`, `map3`과 같은 함수를 사용하여 필요한 데이터만 추출할 수 있습니다. 또한, `Json.Decode.Pipeline` 모듈을 사용하면 더욱 간결하게 JSON 데이터를 파싱할 수 있습니다. 더 많은 정보는 Elm 공식 문서에서 확인할 수 있습니다.

## See Also
- [Elm 공식 문서](https://guide.elm-lang.org/effects/json.html)
- [JSON 데이터 다루기 예제](https://elmprogramming.com/decoding-json-in-elm.html)
- [Elm 커뮤니티](https://discourse.elm-lang.org/)