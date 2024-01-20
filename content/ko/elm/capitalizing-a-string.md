---
title:                "문자열 대문자로 변환하기"
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하죠?)
문자열의 첫 글자를 대문자로 만드는 것을 말해요. 주로 문장을 시작할 때나 제목, 중요 단어를 강조할 때 사용하죠.

## How to:
Elm에선 기본적으로 문자열을 대문자로 만드는 함수가 없어요. 직접 만들어 볼까요?

```Elm
import String exposing (toList, cons, fromList, toUpper)

capitalize : String -> String
capitalize string =
    string
        |> toList
        |> List.head
        |> Maybe.map toUpper
        |> Maybe.withDefault ' '
        |> (\upperCaseHead -> cons upperCaseHead (String.dropLeft 1 string))
        |> fromList

-- 사용 예시
main =
    String.words "hello elm world"
        |> List.map capitalize
        |> String.join " "
        |> Debug.toString
        |> text
-- 출력: "Hello Elm World"
```

## Deep Dive (심층 분석)
Elm에서 문자열을 대문자로 만드는 기본 함수는 없어요. 이는 Elm이 가능한 단순함을 유지하고자 하는 철학 때문이죠. 문자열을 대문자로 만드는 다른 언어의 메소드들과는 달리, 직접 함수를 구현해야 해요.

JavaScript 같은 경우엔 `toUpperCase()` 메소드를 사용하면 되지만, Elm에서는 리스트 변환과 몇 가지 함수 조합을 이용해야 해요.

함수 내부에서는 문자열을 리스트로 바꿔 첫 글자를 추출한 후 대문자로 만듭니다. 그 다음, 원래 문자열에서 첫 글자를 제외한 나머지 문자열과 합쳐요.

## See Also (추가 정보)
- Elm `String` 패키지: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm `Char` 패키지: https://package.elm-lang.org/packages/elm/core/latest/Char
- Elm 커뮤니티 토론: https://discourse.elm-lang.org/