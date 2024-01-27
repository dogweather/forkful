---
title:                "테스트 작성하기"
date:                  2024-01-19
html_title:           "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
테스트 작성이란 코드가 원하는 대로 동작하는지 확인하는 과정입니다. 버그를 줄이고, 코드의 품질을 높이며, 나중에 변경이 생겼을 때 코드가 여전히 잘 동작하는지 검증하기 위해 프로그래머들이 합니다.

## How to:
Elm에서 테스트를 작성하려면 `elm-test` 패키지가 필요합니다. 간단한 예제를 보여드리겠습니다.

```Elm
import Expect exposing (Expectation)
import Test exposing (..)
import ExampleProgram exposing (add)

suite : Test
suite =
    describe "ExampleProgram"
        [ test "add 함수 테스트" <|
            \_ -> 
                2 
                    |> add 2 
                    |> Expect.equal 4
        ]

-- 이 테스트를 실행하면 다음과 같은 결과가 나옵니다.

TEST RUN PASSED

1 test run, all passed
```

## Deep Dive
Elm에서 테스트는 프로그램이 시간과 함께 안정적으로 발전할 수 있도록 돕습니다. Evan Czaplicki가 만든 Elm은 강력한 타입 시스템을 가졌지만 테스팅은 여전히 필요합니다. elm-test는 Noah Hall과 Richard Feldman 등에 의해 개발되었습니다. elm-test의 주요 대안은 없으며, Elm 커뮤니티에서 널리 사용됩니다. 구현 상세로, elm-test는 무작위 데이터 생성 및 퍼지 테스팅을 포함합니다.

## See Also
- [Elm 공식 테스트 가이드](https://package.elm-lang.org/packages/elm-explorations/test/latest)
- [Elm 테스트 Github 저장소](https://github.com/elm-explorations/test)
- [Elm 커뮤니티 예제와 튜토리얼](https://elm-lang.org/examples)
