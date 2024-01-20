---
title:                "문자열 대문자화"
html_title:           "Elm: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열을 대문자화하면, 문자열에서 모든 문자가 대문자로 변환됩니다. 이 작업은 프로그래머들이 텍스트 비교를 단순화하거나 사용자 인터페이스를 표준화하기 위해 자주 수행합니다.

## 이렇게 합시다:

Elm에서 문자열 대문자화는 'String.toUpper' 함수를 사용합니다.

```Elm
import Html exposing (Html, text)
import String

main =
  Html.beginnerProgram { model = "hello, world!", view = view, update = identity }

view model =
  Html.div [] [ Html.text (String.toUpper model) ]
```

위 프로그램을 실행하면, "HELLO, WORLD!"가 출력됩니다.

## 심화 학습 

문자열 대문자화의 이력은 컴퓨터 시스템의 초기부터 있었습니다. 대소문자가 동일한 문자로 간주되기 때문에 정렬이나 검색을 수행할 때 대소문자 구분이 복잡성을 증가시키기 때문입니다.

대안으로, Elm에서는 각각의 문자에 대해 'Char.toUpper' 함수를 호출하고 결과를 다시 문자열로 결합하는 것이 가능합니다. 어느 경우에든, 표준 라이브러리를 사용하는 것이 코드 유지 관리를 쉽게 만듭니다.

'String.toUpper' 함수는 문자열을 반복하고 각 문자에 대해 'Char.toUpper' 함수를 호출합니다. 디테일한 내용과 소스 코드는 Elm 패키지 문서에서 찾을 수 있습니다.

## 참고 링크

Elm 문자열 함수 : [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)  
Elm 문자 함수 : [https://package.elm-lang.org/packages/elm/core/latest/Char](https://package.elm-lang.org/packages/elm/core/latest/Char)