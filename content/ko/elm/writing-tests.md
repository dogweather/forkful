---
title:                "테스트 작성"
html_title:           "Elm: 테스트 작성"
simple_title:         "테스트 작성"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/writing-tests.md"
---

{{< edit_this_page >}}

# 어떻게 테스트를 작성할까요?
테스트를 작성하는 것은 어렵지 않습니다. 더 중요한 것은 왜 프로그래머들이 이 작업을 하는지 이해하는 것입니다. 우리는 테스트를 작성하여 코드의 작동 여부를 확인하고 예상치못한 오류를 방지하기 위해 노력합니다.

## 무엇이며 왜 필요할까요?
테스트는 코드의 작동 여부를 확인하는 프로그래머들의 일부분입니다. 코드가 예상대로 작동하지 않을 때 이를 확인하고 수정하는 데 매우 유용합니다. 이를 통해 우리는 더 나은 코드를 작성할 수 있고 믿을 수 있는 응용 프로그램을 만들 수 있습니다.

## 작성 방법:
테스트를 작성하는 가장 간단한 방법은 `elm-test` 패키지를 사용하는 것입니다. 이 패키지를 사용하여 코드의 예상 입력을 작성하고 실제 결과를 확인할 수 있습니다. 아래는 `elm-test`를 사용하여 테스트를 작성하는 간단한 예제입니다.

```
import Expect
import Test exposing (..)

tests : Test
tests =
    describe "Sum" [
        test "sum 1 and 2 is 3" <|
            \_ -> 
                Expect.equal (1 + 2) 3
    ]
```

위 코드에서 `describe` 함수는 코드의 특정 부분에 대한 설명을 제공하고 `test` 함수는 예상 결과를 작성합니다. 이를 실행하면 `sum 1 and 2 is 3`이 정확하게 작동하는지 확인할 수 있습니다.

## 더 깊이 들어가보기:
테스트는 코드를 작성하는 데 도움이 되는 도구입니다. 이것은 개발 프로세스에서 필수적인 단계입니다. 다른 대안으로는 단위 테스트와 통합 테스트가 있습니다. 또한 `elm-test` 패키지 외에도 `elm-verify-examples`와 같은 도구를 사용하여 코드의 예상된 예제를 작성하고 실행할 수도 있습니다.

## 더 알아보기:
더 많은 정보를 원하시면 아래 링크를 참조하세요.
1. [Elm 공식 사이트](https://elm-lang.org/)
2. [테스트 작성 방법에 대한 블로그 포스트](https://guide.elm-lang.org/testing/)

이제 당신도 테스트를 작성하는 법을 알게 되었습니다. 이제 부터는 더 나은 코드를 작성하는 데 더 이상 걱정하지 않아도 됩니다. 테스트를 이용하여 더 작동이 잘되는 코드를 만들어보세요!