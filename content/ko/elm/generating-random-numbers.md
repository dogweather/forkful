---
title:                "랜덤 숫자 생성하기"
html_title:           "Elm: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

우리는 종종 무작위 수를 생성하는 것이 필요합니다. 예를 들어 게임에서 적, 아이템 또는 레벨을 무작위로 설정하거나 프로젝트에서 테스트 데이터를 만들 때 등등 말이죠. 이러한 경우 우리는 프로그램이나 언어에서 무작위 수를 생성하는 방법을 알아야 합니다. 여기서는 Elm에서 무작위 수를 생성하는 방법을 배우겠습니다.

## 하우 투

### 기본 범위 내에서 무작위 수 생성

먼저, Elm의 Random 라이브러리에 있는 `generate` 함수를 사용하여 일정 범위 내에서 무작위 수를 생성할 수 있습니다. 예를 들어, 1부터 10 사이의 무작위 정수를 생성하는 코드는 다음과 같습니다.

```Elm
import Random

Random.generate (Random.int 1 10)
```

무작위 정수 말고도, `Random.float` 함수를 사용하면 일정 범위 내에서 무작위 소수점 숫자를 생성할 수도 있습니다.

```Elm
import Random

Random.generate (Random.float 0 1)
```

### 사용자 정의 타입에서 무작위 값 생성

우리는 자체적으로 사용자 정의 타입에서 무작위 값을 생성할 수도 있습니다. 예를 들어, `User` 타입에서 무작위 값을 생성하는 코드는 다음과 같습니다.

```Elm
type alias User =
  { name : String
  , age : Int
  }

import Random

userGenerator : Random.Generator User
userGenerator =
  Random.map2 User (Random.string 5) (Random.int 18 60)

Random.generate userGenerator
```

위 코드에서는 `Random.string`과 `Random.int` 함수를 조합하여 `User` 타입의 값들을 무작위로 생성합니다. 따라서 이를 활용하여 프로그램에서 필요한 무작위 데이터를 생성할 수 있습니다.

## 딥 다이브

우리는 이제 무작위 값을 생성하기 위해 어떻게 코드를 작성하는지 알게 되었지만, 실제로는 어떤 방식으로 무작위 수가 생성되는지 알고 있어야 합니다. Elm에서는 Pseudo-Random number generator (PRNG) 알고리즘을 사용하여 무작위 값을 생성합니다. 이 알고리즘은 시드(seed)라고 불리는 시작 숫자에서 시작하여 계속해서 무작위 값을 생성하는 방식입니다. 따라서 같은 시드를 사용하면 항상 같은 무작위 수가 생성되며, 이는 효율적이고 안정된 무작위 값을 생성하는 방법입니다.

## 더 알아보기

- [The Elm Guide: Random](https://guide.elm-lang.org/effects/random.html)
- [Elm Random library documentation](https://package.elm-lang.org/packages/elm/random/latest/)