---
title:                "Elm: 프로그래밍 테스트 작성"
simple_title:         "프로그래밍 테스트 작성"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/writing-tests.md"
---

{{< edit_this_page >}}

## 왜 Elm에서 테스트를 작성해야 하는가?

안녕하세요! Elm 개발자 여러분들, 오늘은 Elm에서 테스트를 작성하는 이유에 대해 알아보려고 합니다.

대부분의 개발자들이 테스트 코드를 작성하기 귀찮아 하지만, 테스트 코드는 프로그램을 안정적으로 유지하고 개발 과정에서 버그를 미리 발견할 수 있는 중요한 요소입니다. 또한 Elm에서는 테스트 코드를 작성함으로써 보다 신뢰성 있는 코드를 작성할 수 있으며, 유지보수가 더욱 쉬워집니다. 그러니까, Elm 프로젝트에서는 꼭 테스트 코드를 작성하는 것을 권장합니다!

## 작성 방법

이제 실제로 Elm에서 테스트 코드를 작성해보도록 하겠습니다. Elm의 테스트 모듈을 사용하면 간단하고 쉽게 테스트 코드를 작성할 수 있습니다.

먼저, 다음과 같이 테스트 모듈을 불러옵니다.

```
Elm.Test
```

그리고 다음과 같이 `test` 함수를 사용하여 테스트를 정의할 수 있습니다.

```
test "테스트 이름" <|
    \() -> 
        ...
```

위의 코드에서 `테스트 이름` 부분에는 테스트의 이름을 입력하고, `\()` 부분에는 테스트 코드를 작성하면 됩니다.

그리고 마지막으로 `Test.suite` 함수를 사용하여 테스트 스위트를 만들 수 있습니다.

```
Test.suite "테스트 스위트 이름"
    [ test "첫 번째 테스트" <|
        \() -> 
            ...
    , test "두 번째 테스트" <|
        \() -> 
            ...
    ]
```

위의 코드에서 `테스트 스위트 이름` 부분에는 테스트 스위트의 이름을 입력하고, `[ ]` 안에는 앞서 작성한 테스트들의 리스트를 입력하면 됩니다.

자, 이제 실제로 예제 코드를 작성해보도록 하겠습니다.

```
module Example exposing (..)

import Html exposing (..)
import Elm.Test

square : Int -> Int
square x =
    x * x

failureError : Elm.Test.FailureCase
failureError =
    Elm.Test.error "제곱 결과가 예상과 다릅니다."

five : Elm.Test.OkCase
five =
    Elm.Test.ok "5의 제곱은 25입니다."
        (square 5)
        25

six : Elm.Test.OkCase
six =
    Elm.Test.ok "6의 제곱은 36입니다."
        (square 6)
        36

suite : Test.Test
suite =
    Test.suite "제곱 함수 테스트"
        [ five
        , six
        , failureError
        ]

main : Program Never Model Msg
main =
    Html.text "테스트 결과를 확인하려면 Console을 열어보세요."
```

위의 코드에서는 `square` 함수를 테스트하고 있습니다. `failureError`는 의도적으로 실패하는 테스트를 위한 함수이며, `five`와 `six`는 제대로 작동하는 경우를 위한 함수입니다.

마지막으로 `main` 함수에서는 테스트 결과를 Console에서 확인할 수 있도록 설정하였습니다. 코드를 실행해보면 다음과 같이 테스트 결과가 출력됩니다.

```
테스트 스위트 지수 함수 테스트

Ok (명령어 {command = (), description = "5의 제곱은 25입니다."})
Ok (명령어 {command = (), description = "6의 제곱은 36입니다."})
Expected (error "예상과 다른 제곱 결과" <fakeOuterFunction>-:1:1)

11개 테스트에 실패하였습니다.