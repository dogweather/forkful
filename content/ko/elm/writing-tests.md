---
title:    "Elm: 테스트 작성하기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

테스트를 작성하는 것에 참여하는 이유는 무엇일까요? 테스트를 작성함으로써 코드의 신뢰성을 높이고 버그를 방지할 수 있습니다. 또한 작성한 테스트는 코드 수정 및 리팩토링 시 도움이 될 수 있습니다.

## 작성하는 방법

테스트를 작성하는 방법은 매우 간단합니다. 우선 ```elm-test``` 패키지를 설치해야 합니다. 그리고 코드에서 테스트할 함수를 불러오고, ```expect``` 함수를 사용하여 예상되는 결과를 입력하면 됩니다. 아래는 간단한 예제 코드입니다.

```elm
module Example exposing (..)

import Expect
import Test

divide : Int -> Int -> Int
divide a b =
    a // b

tests =
    describe "Divide function"
        [ test "Correct result" <| \() ->
            expect (divide 10 2) |> toEqual 5
        , test "Zero division" <| \() ->
            expect (divide 5 0) |> toEqual 0
        ]

main =
    Test.run tests
```

위 코드를 실행하면 아래와 같은 결과를 볼 수 있습니다.

```
Divide function
❌ Correct result
Expected 5, but got 10

😁 Zero division
Passed 0 out of 1 tests
Ran 2 tests of 2 total
```

예상한 결과와 일치하지 않는 경우 잘못된 값이 출력됩니다. 이렇게 테스트를 작성하면 코드 수정 시 예상한 결과와 다른 값이 나오는지 확인할 수 있습니다.

## 더 깊게 들어가기

테스트를 작성하는 더 깊은 내용은 여러 가지가 있습니다. 예를 들어, 테스트 케이스에서 임의의 값을 생성할 수 있는 ```Random``` 모듈을 사용할 수 있습니다. 또한 ```describe``` 함수를 사용하여 테스트 케이스를 그룹화하고, ```only``` 함수를 사용하여 특정 테스트 케이스만 선택적으로 실행할 수도 있습니다.

더 많은 정보는 공식 Elm 문서를 참고하시기 바랍니다.

## 더 알아보기

이 글에서는 간단한 예제 코드를 통해 Elm에서 테스트를 작성하는 방법에 대해 알아보았습니다. 더 많은 정보를 알고 싶다면 아래의 링크들을 참고하시기 바랍니다.

#### 참고 링크

- https://guide.elm-lang.org/testing/
- https://package.elm-lang.org/packages/elm-explorations/test/latest/
- https://dev.to/petrbela/unit-testing-in-elm-4g86 (영문)
- https://medium.com/@kristyjy/elm-unit-testing-for-side-effects-e1c5bf8be049 (영문) 

## 더 알아보기