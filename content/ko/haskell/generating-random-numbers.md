---
title:    "Haskell: 랜덤 숫자 생성하기"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 왜

난수를 생성하는 것에 대해 관심을 가지는 이유는 다양할 수 있습니다. 일단, 난수 생성은 암호학에서 중요한 역할을 합니다. 또한 게임이나 시뮬레이션 등에서 쉽고 빠르게 무작위 값을 생성하는 것이 필요할 때가 있을 수 있습니다. 그리고 심지어는 무엇을 먹을지 정하는 데에도 난수를 사용할 수 있습니다!

## 사용 방법

Haskell에서 난수를 생성하려면, `random` 패키지를 사용해야 합니다. 우선 패키지를 import해야 합니다:

```Haskell
import System.Random
```

그런 다음, `StdGen` 객체를 생성하는 함수인 `mkStdGen`을 사용합니다. 이 객체는 난수 생성기의 상태를 나타내며, 이를 기반으로 난수 값을 생성할 수 있습니다. 예를 들어, `mkStdGen 123`을 실행하면 매번 같은 순서로 난수를 생성할 수 있습니다.

다음은 `getRandom` 함수를 사용하여 `Int` 형의 난수 값을 생성한 후 출력하는 예시입니다:

```Haskell
let gen = mkStdGen 123
let (value, newGen) = random gen :: (Int, StdGen)
print value
-- 출력 결과: -3762966853418755567
```

또 다른 예시로, `randomR` 함수를 사용하여 원하는 범위 내에서 난수 값을 생성하는 방법도 있습니다. 다음은 1부터 10 사이의 난수 값을 생성하는 예시입니다:

```Haskell
let gen = mkStdGen 123
let (value, newGen) = randomR (1, 10) gen :: (Int, StdGen)
print value
-- 출력 결과: 6
```

## 깊이 들어가기

난수 생성의 원리는 매우 복잡하지만, 우리는 이를 단순하게 생각할 수 있습니다. Haskell의 `random` 패키지는 매우 높은 수준의 알고리즘을 사용하여 난수 값을 생성합니다. 이는 암호학적으로 안전하고 균일한 분포를 가진 난수를 생성할 수 있도록 보장합니다. 또한 `StdGen` 객체를 업데이트하여 매번 다른 난수 값을 생성할 수 있도록 합니다.

조금 더 깊이 들어가보면, `StdGen`은 초기화된 seed 값을 내부 상태로 가지고 있으며, 이를 기반으로 난수 값을 생성합니다. 따라서 매번 같은 seed 값이 주어지면 항상 같은 난수 값이 생성됩니다. 이것이 난수의 핵심적인 특징 중 하나입니다.

## 관련 자료

- [Haskell 공식 문서 - 난수 생성](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [블로그 - Haskell에서 난수를 생성하고 게임 만들기](https://www.schoolofhaskell.com/user/bssullivan/using-haskell-from-c-interop/randoms)