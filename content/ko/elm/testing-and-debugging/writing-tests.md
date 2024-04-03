---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:32.959810-07:00
description: "Elm\uC5D0\uC11C \uD14C\uC2A4\uD2B8 \uC791\uC131\uC740 Elm \uCF54\uB4DC\
  \uC758 \uC815\uD655\uC131\uC744 \uAC80\uC99D\uD558\uB294 \uD14C\uC2A4\uD2B8 \uCF00\
  \uC774\uC2A4\uB97C \uC791\uC131\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\
  \uB2E4. \uC774\uB294 \uCF54\uB4DC\uAC00 \uC608\uC0C1\uB300\uB85C \uB3D9\uC791\uD558\
  \uB294\uC9C0 \uD655\uC778\uD558\uAE30 \uC704\uD568\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uBC84\uADF8\uB97C \uC870\
  \uAE30\uC5D0 \uBC1C\uACAC\uD558\uACE0, \uC720\uC9C0 \uBCF4\uC218\uB97C \uC6A9\uC774\
  \uD558\uAC8C \uD558\uBA70, \uC751\uC6A9 \uD504\uB85C\uADF8\uB7A8\uC758 \uD488\uC9C8\
  \uACFC \uC2E0\uB8B0\uC131\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uB824\uACE0\u2026"
lastmod: '2024-03-13T22:44:55.115510-06:00'
model: gpt-4-0125-preview
summary: "Elm\uC5D0\uC11C \uD14C\uC2A4\uD2B8 \uC791\uC131\uC740 Elm \uCF54\uB4DC\uC758\
  \ \uC815\uD655\uC131\uC744 \uAC80\uC99D\uD558\uB294 \uD14C\uC2A4\uD2B8 \uCF00\uC774\
  \uC2A4\uB97C \uC791\uC131\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4\
  ."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 무엇 & 왜?

Elm에서 테스트 작성은 Elm 코드의 정확성을 검증하는 테스트 케이스를 작성하는 것을 포함합니다. 이는 코드가 예상대로 동작하는지 확인하기 위함입니다. 프로그래머들은 이를 통해 버그를 조기에 발견하고, 유지 보수를 용이하게 하며, 응용 프로그램의 품질과 신뢰성을 향상시키려고 합니다.

## 방법:

Elm은 단위 테스트 및 퍼즈 테스트를 작성하기 위해 `elm-explorations/test` 패키지를 사용합니다. 패키지를 프로젝트에 추가하려면 다음을 시작하세요:

```elm
elm install elm-explorations/test
```

테스트 파일을 생성하세요. 예를 들어 `tests/ExampleTest.elm`이라고 하고, 테스트 모듈을 가져옵니다. 다음은 함수 `add : Int -> Int -> Int`를 검증하는 간단한 테스트입니다:

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "A simple addition function"
        [ test "Adding 2 and 3 yields 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

테스트를 실행하려면 `elm-test`가 필요합니다:

```shell
npm install -g elm-test
elm-test
```

이렇게 하면 테스트가 컴파일되고 결과가 터미널에 출력됩니다. 위의 예시에서 출력은 다음과 같아야 합니다:

```
TEST RUN PASSED

Duration: 42 ms
Passed:   1
Failed:   0
```

더 복잡한 예시를 살펴보겠습니다. 넓은 범위의 정수 입력을 올바르게 처리하는지 확인하기 위해 `add` 함수를 퍼즈 테스트하고 싶다고 가정해 보세요. `ExampleTest.elm`을 다음과 같이 수정하면 됩니다:

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Testing add with fuzzing"
        [ fuzz int "Fuzz testing add with random ints" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

`elm-test`를 다시 실행하여 퍼즈 테스트를 확인하세요. 랜덤 입력에 따라 출력은 달라지겠지만, 성공적인 테스트는 실패가 없음을 나타냅니다:

```
TEST RUN PASSED

Duration: 183 ms
Passed:   100
Failed:   0
``` 

이 예시들은 `elm-explorations/test` 패키지를 사용하여 Elm에서 간단한 단위 및 퍼즈 테스트를 작성하고 실행하는 방법을 보여줍니다. 테스트는 개발 과정에서 중요한 부분으로, Elm 애플리케이션이 신뢰성을 유지하고 높은 품질을 보장하는 데 도움이 됩니다.
