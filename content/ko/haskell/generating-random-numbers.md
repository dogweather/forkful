---
title:    "Haskell: 랜덤 숫자 생성"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

랜덤한 숫자를 생성하는 일에 참여할 이유를 설명합니다.

Generating random numbers can be useful in many situations, such as creating randomized test data or adding an element of unpredictability to a program.

## 사용 방법

다음은 Haskell로 랜덤한 숫자를 생성하는 간단한 코드 예제입니다.

```Haskell
import System.Random

main = do
  gen <- getStdGen
  let (randNum, newGen) = randomR (1, 10) gen :: (Int, StdGen)
  print randNum
```

위 코드는 `randomR` 함수를 사용해 1부터 10 사이의 랜덤한 정수를 생성합니다. `getStdGen` 함수로 랜덤 생성기를 가져오고, 그 생성기를 이용해 랜덤한 숫자를 생성합니다. 결과는 `randNum` 변수에 저장되고, `print` 함수로 출력됩니다.

다른 방식으로는 `random` 함수를 사용하는 방법이 있습니다.

```Haskell
import System.Random

main = do
  gen <- getStdGen
  let (randNum, newGen) = random gen :: (Double, StdGen)
  print randNum
```

여기서는 `random` 함수를 사용하여 0부터 1 사이의 랜덤한 실수를 생성합니다. 위 코드는 정수를 생성하는 예제와 동일하게 `print` 함수를 이용해 결과를 출력합니다.

`random`과 `randomR`이 아닌 더 많은 랜덤 생성 함수가 있으며, 사용 방법은 Haskell 공식 문서를 참고하시면 됩니다.

## 깊이 파고들기

Haskell에서는 랜덤 숫자를 생성하기 위해 랜덤 생성기를 사용합니다. 랜덤 생성기는 상태를 갖고 있어 다음에 생성될 랜덤한 수를 계산할 수 있습니다. `getStdGen` 함수로 기본적으로 제공되는 랜덤 생성기를 사용할 수 있지만, 사용자가 직접 랜덤 생성기를 생성해서 사용할 수도 있습니다.

또한 Haskell에서는 랜덤 숫자를 생성하고 관리하기 위한 `Random` 라이브러리가 있습니다. 이 라이브러리를 이용하면 여러 종류의 데이터 타입에 대해 랜덤 숫자를 생성할 수 있으며, 생성된 랜덤 숫자를 다른 타입으로 변환할 수도 있습니다.

더 자세한 내용은 Haskell 공식 문서나 책을 참고하시면 됩니다.

## 그밖에 알아볼만한 자료들

- [Haskell 공식 문서 - 랜덤 모듈](https://hackage.haskell.org/package/random)
- [Real World Haskell - 랜덤 수 생성하기](http://book.realworldhaskell.org/read/programming-with-monads.html#monads.random)
- [Haskell Wiki - 랜덤 수 생성하기](https://wiki.haskell.org/Random_number_generation)