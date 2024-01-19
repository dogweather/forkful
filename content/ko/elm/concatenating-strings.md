---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 사용하는가?
문자열을 연결한다는 것은 여러 문자열을 합쳐 하나의 문자열로 만드는 것입니다. 이는 프로그래머들이 데이터를 시각적으로 구성하거나 메시지를 구축할 때 필요한 작업입니다.

## 어떻게 사용하는가:
아래의 예시를 통해 문자열 연결을 확인해봅시다.

```Elm
module Main exposing (..)
import Html exposing (text)

main =
    let
        name1 = "코딩"
        name2 = "세션"
    in
    Html.beginnerProgram { model = name1 ++ " " ++ name2, view = text, update = \_ _ -> "" }
```

프로그램을 실행하면, output 영역에서 "코딩 세션"이라는 결과를 확인할 수 있습니다.

## 깊게 알아보기
문자열 연결은 컴퓨터 프로그래밍의 근간 중 하나이며, 이는 거의 모든 프로그래밍 언어에서 구현되어 있습니다. Elm에서는 '++' 연산자를 통해 이를 구현하고 있습니다. 문자열 연결 대신에 문자열 포맷팅이나 문자열 보간법을 사용하는 것도 가능합니다. 

구현 세부적으로, Elm의 문자열 연결은 속도와 효율성을 높이기 위해 목록의 연결(concatenation of lists) 방식으로 내부적으로 구현되어 있습니다.

## 참조하기
추가로 학습하실 분들은 아래 링크를 참조하세요.
- Elm 공식 문서: [https://elm-lang.org/docs](https://elm-lang.org/docs) 
- Elm 문자열 연결에 대한 자세한 설명: [https://package.elm-lang.org/packages/elm/core/latest/String#concat](https://package.elm-lang.org/packages/elm/core/latest/String#concat)