---
title:                "정규식 사용하기"
html_title:           "Elm: 정규식 사용하기"
simple_title:         "정규식 사용하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하는 이유는 매우 유용하고 강력하기 때문입니다. 정규 표현식을 사용하면 텍스트를 처리하고 분석하는 데 도움이 되며, 데이터 정제, 검색 및 변환 작업에 도움이 될 수 있습니다.

## 사용 방법

정규 표현식을 사용하기 위해서는 먼저 `elm/parser`라이브러리를 설치해야 합니다. 아래 예제는 단어가 들어있는 문자열에서 대문자로 시작하는 모든 단어를 찾아서 리스트로 반환하는 방법을 보여줍니다.

```elm
import Parser exposing (..)
import Regex exposing (..)

findUpperCaseWords : String -> List String
findUpperCaseWords input =
    let
        uppercaseWord = regex "[A-Z]\\w+" -- 여기에서 정규 표현식을 작성합니다.
        words = oneOf [ uppercaseWord, string "." ] -- 단어 또는 마침표가 들어있는 리스트를 반환합니다
    in
    case run words input of
        Ok capturedTexts -> capturedTexts
        Err _ -> []

findUpperCaseWords "Hello World, How Are You?" -- 결과: ["Hello", "World", "How", "Are", "You"]
```

## 깊이있는 이해

정규 표현식에는 패턴과 함께 매칭되는 문자열을 찾아내는 규칙이 포함됩니다. `elm/parser` 라이브러리의 `regex` 함수를 통해 정규 표현식을 작성하고, `oneOf`를 사용하여 원하는 패턴을 정의할 수 있습니다. 또한, `run` 함수를 사용하여 입력 문자열에서 찾고 싶은 패턴을 캡처할 수 있습니다.

## 연관 글

* [Regular Expressions in Elm](https://dev.to/gizra/regular-expressions-in-elm-2m5b)
* [Regular Expressions in Elm Tutorial](https://www.codementor.io/@sdraschner/regular-expressions-in-elm-tutorial-dyflym7np)
* [elm/parser documentation](https://package.elm-lang.org/packages/elm/parser/latest/)