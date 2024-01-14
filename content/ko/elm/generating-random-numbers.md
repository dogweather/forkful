---
title:    "Elm: 랜덤 숫자 생성하기"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 왜?

 랜덤 숫자를 생성하는 것이 왜 유익한지 궁금하신가요? 두 가지 이유가 있습니다. 첫째, 랜덤 숫자는 게임 및 모의 시뮬레이션 등의 애플리케이션에서 필수적입니다. 둘째, 랜덤 숫자를 사용하면 흥미로운 데이터 시각화도 가능합니다.

## 만드는 방법

 Elm에서 랜덤 숫자를 만드는 것은 매우 간단합니다. 우선, 랜덤 숫자를 사용하기 위해서는 Elm의 랜덤 모듈을 import해야 합니다. 그리고 `random` 함수를 사용하여 원하는 범위 내에서 랜덤 숫자를 생성할 수 있습니다. 아래의 예제 코드를 참고해보세요.

```elm
import Random

random_number = 
    Random.float 0 1 -- 0부터 1까지의 랜덤한 소수를 생성
    |> Random.map (* 100) -- 100을 곱하여 정수로 변환
    |> Random.generate NumberGenerated -- NumberGenerated 메시지를 생성하여 사용 가능

```

위의 예제 코드에서는 `Random.map` 함수를 사용하여 랜덤 숫자를 정수로 변환하고, `Random.generate` 함수를 사용하여 액션을 생성해줍니다. 이제 이 액션을 메시지로 받아서 활용할 수 있습니다.

## 깊이 살펴보기

 Elm에서 랜덤 숫자를 생성하는 방법은 위의 예제 코드처럼 간단하지만, 내부적으로는 어떻게 동작할까요? Elm에서는 `seed`를 사용하여 랜덤 값을 생성합니다. `Random.seed` 함수를 사용하여 `Seed` 타입의 값을 생성한 다음, 이 값을 `Random.step` 함수에 넘겨주면 새로운 `Seed` 값과 원하는 타입의 랜덤 값이 반환됩니다.

예를 들어, `Random.step (float 0 1) seed`를 사용하면 0부터 1까지의 랜덤한 소수가 생성되고, 새로운 `Seed` 값이 반환됩니다. 이를 점점 반복하면 다양한 랜덤 값들을 생성할 수 있습니다.

## 더 알아보기

- Elm 랜덤 모듈 문서: https://package.elm-lang.org/packages/elm/random/latest/
- Elm 표준 라이브러리 문서: https://package.elm-lang.org/packages/elm/core/latest/ 
- 랜덤 숫자 예제 애플리케이션: https://github.com/elm/random/tree/1.0.0/examples/Random-Word