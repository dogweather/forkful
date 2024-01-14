---
title:    "Elm: 테스트 작성하기"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 왜

왜 테스트를 작성하는 것에 참여해야 할까요? 테스트를 작성하면 코드를 안전하게 유지하는 데 도움이 됩니다. 또한 나중에 코드를 변경할 때 예상치 못한 부작용을 방지하는 데도 도움이 됩니다.

## 작성 방법

```Elm
module Main exposing (..)

import Html exposing (..)
import Expect

-- 테스트할 함수
split string =
    String.split " " string


-- 테스트 코드
test_split =
    describe "split 함수" <|
        [ test "문자열을 분리할 수 있어야 함" <|
            \_ ->
                Expect.equal (split "Hello World") ["Hello", "World"]
        , test "빈 문자열일 때 빈 리스트를 반환해야 함" <|
            \_ ->
                Expect.equal (split "") []
        ]
```

위 코드는 `split` 함수를 테스트하는 예제입니다. `describe`로 함수의 이름을 지정하고, `test`로 해당 기능에 대한 단언문을 작성합니다. `Expect.equal`을 사용하여 예상되는 결과와 실제 결과를 비교해보는 것이 중요합니다.

## 깊게 들어가기

테스트를 작성할 때 고려해야 할 몇 가지 중요한 사항이 있습니다. 첫 번째로는 어떤 값을 테스트할 것인지 결정하는 것입니다. 가능한 모든 경우에 대해 테스트를 작성하는 것은 불가능하기 때문에 가장 중요한 기능을 포함해야 합니다. 그리고 기능을 테스트할 때는 모든 가능성을 다루도록 충분한 테스트 케이스를 작성해야 합니다.

또한 테스트에서 예외 상황을 다루는 것도 중요합니다. 함수가 예외 상황일 때 적절한 예외를 발생시키는지 테스트하는 것이 좋습니다.

마지막으로, 테스트를 작성하고 통과시킬 때는 매우 적극적으로 코드를 디버깅해야 합니다. 여러분의 함수가 테스트를 통과하도록 만드는 데에 집중하지 말고, 올바른 결과를 반환하도록 고치는 데 집중해야 합니다.

## 더 알아보기

### Elm 테스트 시작하기
https://guide.elm-lang.org/testing/

### Elm 테스트 관련 팁
https://github.com/elm-community/elm-test#elm-test-tips

### 테스트 시나리오 작성 방법
https://thoughtbot.com/blog/dealing-with-asynchronous-ui-tests