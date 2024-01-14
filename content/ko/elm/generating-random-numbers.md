---
title:                "Elm: 랜덤 숫자 생성하기."
simple_title:         "랜덤 숫자 생성하기."
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜?

랜덤한 숫자를 생성하는 것에 대해 궁금했던 적이 있나요? 바로 이것 때문이죠. 랜덤하게 생성된 숫자를 사용하면 애플리케이션에 다양한 놀이요소 및 기능을 추가할 수 있고, 더욱 흥미롭고 다양한 경험을 제공할 수 있습니다.

## 어떻게?

이제 Elm으로 랜덤 숫자를 생성하는 방법을 알아보겠습니다. 먼저, Elm 패키지에서 Random Generator를 가져와야 합니다.

```Elm
import Random

```

랜덤 숫자를 생성하기 위해선 `generator` 함수를 사용해야 합니다. 이 함수는 생성한 랜덤 숫자를 `Seed` 타입으로 반환합니다. 그리고 `initialSeed` 함수를 사용해 초기 랜덤 `Seed`를 생성합니다.

```Elm
Random.generator Random.int
    |> Random.map (+1)
    |> Random.initialSeed
```

위의 코드에서는 `Random.int`를 사용해 랜덤한 정수를 생성한 후, `map` 함수를 사용해 숫자를 1만큼 증가시키고, `initialSeed` 함수를 사용해 초기 `Seed`로 변환합니다.

이제 `next` 함수를 사용해 랜덤 숫자를 계속 생성해보겠습니다.

```Elm
Random.next Random.int
    |> Random.map (\(value, seed) -> value + 1)
```

위의 코드에서는 `Random.next` 함수를 사용해 이전에 생성한 `Seed`로부터 새로운 랜덤 숫자를 생성하고, `map` 함수를 사용해 숫자를 1만큼 증가시킵니다.

## 더 깊게 들어가보기

랜덤 숫자를 생성하는 데에는 다양한 방법이 있습니다. Elm에서는 `Random.integer` 함수를 사용해 범위 내에서 랜덤한 정수를 생성할 수 있습니다. 또한 `Random.float` 함수를 사용해 0부터 1사이의 랜덤한 소수를 생성할 수도 있습니다.

또한, `Random.sequence` 함수를 사용해 여러 개의 랜덤 숫자를 한 번에 생성할 수도 있습니다.

```Elm
let
    generator =
        Random.sequence
            [ Random.int 1 10
            , Random.float 0 1
            ]

    (numbers, newSeed) =
        Random.step generator seed
in
    numbers
```

위의 코드에서는 `Random.sequence` 함수를 사용해 랜덤한 정수와 소수를 생성하고, `step` 함수를 사용해 `Seed`를 업데이트하고, 생성된 숫자들을 튜플로 반환합니다.

## 더 알아보기

Elm에서 랜덤 숫자를 생성하는 데에는 다양한 방법들이 있습니다. 이 외에도 `Random.floatRange`, `Random.intRange`, `Random.bool`, `Random.string` 등 다양한 함수를 사용할 수 있습니다.

더 자세한 내용은 Elm 공식 문서를 참고해주세요.

## 더 알아보기

- [Random Generator Elm 패키지](https://package.elm-lang.org/packages/elm/random/latest)
- [Elm 공식 문서 - 랜덤 생성기 모듈](https://guide.elm-lang.org/random/)

# 참고

- [Elm 샘플 코드](https://github.com/kamikaze/awesome-elm#random)
- [모던 Elm 프로그래밍: Function을 사용한 랜덤 숫자 생성](https://thoughtbot.com/blog/making-random-unpredictable-numbers-in-elm-using-functions)