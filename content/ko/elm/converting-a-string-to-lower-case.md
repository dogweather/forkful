---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열을 소문자로 변환하는 것은 모든 대문자를 소문자로 변경하는 것을 의미합니다. 프로그래머들은 일관성을 유지하거나 케이스 민감성에서 오는 오류를 변경하기 위해 이를 수행합니다.

## 어떻게 하나:

Elm에서 문자열을 소문자로 변경하기 위해 `String.toLower` 함수를 사용할 수 있습니다.

```Elm
import Html exposing (text)
import String

main =
  text (String.toLower "HELLO ELM!")
```

이 코드의 출력은 `hello elm!` 입니다.

## 깊은 탐색:

문자열을 소문자로 변경하는 것은 오래 전부터 프로그래밍에서 흔히 사용되는 기능입니다. Elm에서는 `String.toLower` 함수로 이 기능을 제공합니다.

대안으로 `String.foldl`을 사용하여 직접 구현할 수 있으나, 이는 번거롭고 불필요한 작업입니다.

```Elm
import String exposing (foldl)
import Char exposing (toLower)

lowerCase : String -> String
lowerCase = foldl (\x y -> String.fromChar (toLower x) ++ y) ""

main = 
    print (lowerCase "HELLO ELM!")
```

이 코드의 출력은 이전의 `String.toLower` 함수를 사용한 것과 동일하게 `hello elm!`입니다.

## 참고하기:

더 깊게 파보고 싶다면 아래 링크를 참고하세요.
* Elm 문자열 변환 기능 : [Elm String 기능](https://package.elm-lang.org/packages/elm/core/latest/String)
* Elm의 `foldl` 함수 : [foldl 함수](https://package.elm-lang.org/packages/elm/core/latest/List#foldl)