---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열에서 부분 문자열을 추출하는 것은 정해진 문자열에서 필요한 일부분만을 선택하여 사용하는 것을 말합니다. 이는 자주 발생하는 상황에서 필요한 특정 정보를 가져오거나, 너무 많은 정보를 필터링하는데 유용합니다. 

## 어떻게 하는가:

Elm에서는 `String.slice` 함수를 사용하여 부분 문자열을 추출할 수 있습니다. 아래는 간단한 예시입니다:

```Elm
import Html exposing (text)
import String

main =
  text (String.slice 0 5 "Hello, Elm!")
```
이 코드의 출력은 "Hello"입니다.

## Deep Dive:

Elm에서 문자열을 잘라내는 `String.slice` 함수는 JavaScript의 `substring` 메서드에서 영향을 받았습니다. 이는 프로그래밍에서 자주 사용되는 기능이며, 사실상 모든 언어에서 이와 유사한 기능을 제공하고 있습니다.

`String.slice`의 대안으로는 `String.left`나 `String.right`가 있습니다. 이 함수들은 문자열의 시작 또는 끝에서부터 주어진 갯수만큼의 문자를 반환합니다.

Elm의 `String.slice`는 문자열의 인덱스가 0으로 시작하는 점에서 JavaScript와 약간 다릅니다. 이는 Elm이 문자열을 1차원의 문자 배열로 구현하고 있기 때문입니다.

## 참고하면 좋을 자료:

- Elm의 공식 라이브러리 문서에서 `String.slice`의 사용법을 좀 더 자세히 알아볼 수 있습니다: [String.slice](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
- `String.left`와 `String.right`에 대한 추가 정보는 여기에서 찾아보실 수 있습니다: [String.left](https://package.elm-lang.org/packages/elm/core/latest/String#left), [String.right](https://package.elm-lang.org/packages/elm/core/latest/String#right)