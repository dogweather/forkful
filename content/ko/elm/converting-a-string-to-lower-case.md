---
title:                "문자열을 소문자로 바꾸기"
html_title:           "Elm: 문자열을 소문자로 바꾸기"
simple_title:         "문자열을 소문자로 바꾸기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Elm에서 문자열을 소문자로 변환하기

## 무엇인가요? 왜 해야 하나요?
문자열을 소문자로 변환하는 것은 주어진 문자열을 모두 소문자로 바꾸는 작업을 말합니다. 프로그래머들은 대소문자를 구분하는 언어를 다룰 때 이 작업이 필요할 수 있습니다.

## 방법:
Elm에서는 이 작업을 간단하게 할 수 있는 내장 함수 `String.toLower`을 제공합니다. 이 함수는 문자열을 인자로 받아 소문자로 변환된 새로운 문자열을 반환합니다. 예시를 살펴보겠습니다.
```Elm
import String exposing (toLower)

myString = "Hello, World!"
lowercased = toLower myString

-- 출력: "hello, world!"
```
세 번째 줄에서 `toLower` 함수를 사용해 `myString` 변수를 소문자로 변환하고, 그 값을 `lowercased` 변수에 저장합니다. 그 다음 인라인 주석에서 확인할 수 있듯이, `lowercased` 변수의 값은 `"hello, world!"`가 됩니다.

## 깊게 들어가기:
이 작업을 처음부터 구현해보자면, 각 문자의 아스키 코드 값을 확인해 대문자인 경우 32를 빼는 식으로 구현할 수 있습니다. 하지만 `String.toLower` 함수처럼 미리 구현된 함수를 사용하면 훨씬 간단하고 효율적으로 처리할 수 있습니다. Elm에서는 대소문자를 무시하는 비교 함수 `String.CaseInsensitive.compare`를 사용해 두 문자열을 비교할 수도 있습니다.

## 관련 자료:
- [Elm 공식 문서](https://package.elm-lang.org/packages/elm/core/latest/String#FoldCToLower)
- [문자열 다루기: Elm로 구현하기](https://medium.com/stackfame/elm-string-tutorial-5117c5cc080d)