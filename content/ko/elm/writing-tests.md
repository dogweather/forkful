---
title:                "Elm: 테스트 작성하기"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

프로그래밍을 할 때 우리는 종종 버그를 발견하게 됩니다. 이러한 버그로 인해 소프트웨어가 예기치 않게 동작하거나 시스템이 충돌할 수 있습니다. 이러한 문제들을 방지하기 위해서는 **테스트**가 필수적입니다. 테스트를 작성함으로써 이메일을 보낼 때나 웹사이트를 만들 때 쉽게 예측할 수 없는 버그를 발견할 수 있습니다.

## 어떻게 작성할까요?

테스트를 작성하는 것은 어려울 수 있지만, Elm 언어를 사용하면 쉽게 작성할 수 있습니다. 아래의 코드 블록을 보면서 함께 따라해보세요.

```elm
import Html exposing (div, text)


-- 테스트를 위한 add 함수
add : Int -> Int -> Int
add x y =
    x + y


main =
    div []
        [ text (toString (add 2 3)) -- 결과는 5가 나와야 합니다.
        ]
```

위의 코드를 실행하면 `5`가 나와야 합니다. 하지만 새로운 버전의 `add` 함수를 아래와 같이 변경하면 결과는 `6`이 나와야 합니다.

```
add : Int -> Int -> Int
add x y =
    x + y + 1
```

이렇게 테스트를 작성하면 어떤 버그가 발생하는지 쉽게 알 수 있습니다.

## 깊이 파고들기

물론 테스트는 프로그래밍에 대해 깊이 이해하고 있는 것을 요구합니다. 하지만 Elm 언어를 사용하면 테스트를 쉽게 작성할 수 있습니다. 이 외에도 Elm 언어에는 테스트를 작성하기 위한 다양한 함수와 기능들이 있습니다. 더 깊이 공부하면서 더욱 효율적인 테스트를 작성해보세요.

## 더 알아보기

"See Also"로 추가로 알아볼만한 링크들을 모아보았습니다.

- [Elm 공식 홈페이지](https://elm-lang.org/)
- [Elm 테스트 관련 문서](https://guide.elm-lang.org/testing/)