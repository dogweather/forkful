---
title:                "Haskell: 난수 생성하기"
simple_title:         "난수 생성하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

랜덤한 숫자를 생성하는 것에 대해 아마도 궁금해 할 것입니다. 일반적으로 우리는 무작위성을 좋아하지 않지만 프로그래밍에서 랜덤한 숫자를 사용하는 것은 매우 유용합니다. 예를 들어, 재미있는 게임을 만들거나 데이터를 무작위로 샘플링하거나 모의실험을 할 때 랜덤한 숫자는 필수적입니다.

## 사용 방법

Haskell에서 랜덤한 숫자를 생성하는 방법은 매우 간단합니다. 먼저 "System.Random" 모듈을 임포트해야합니다. 그런 다음 "randomRIO" 함수를 사용하여 원하는 범위 내에서 랜덤한 숫자를 생성할 수 있습니다. 아래는 1부터 10까지의 랜덤한 숫자를 생성하는 예제 코드입니다.

```Haskell
import System.Random

main = do
  num <- randomRIO (1, 10)
  putStrLn $ "랜덤한 숫자: " ++ show num
```

출력 예시:

```
랜덤한 숫자: 5
```

또 다른 방법으로는 "random" 함수를 사용하여 임의의 시작값과 랜덤 씨드를 제공하여 랜덤한 숫자를 생성하는 것입니다. 아래는 1부터 10까지의 랜덤한 숫자를 생성하는 예제 코드입니다.

```Haskell
import System.Random

main = do
  gen <- getStdGen
  let (num, _) = randomR (1, 10) gen :: (Int, StdGen)
  putStrLn $ "랜덤한 숫자: " ++ show num
```

출력 예시:

```
랜덤한 숫자: 8
```

또한 "randomRs" 함수를 사용하여 리스트 형태로 여러 개의 랜덤한 숫자를 한 번에 생성할 수도 있습니다. 아래는 1부터 10까지의 랜덤한 숫자 3개를 생성하는 예제 코드입니다.

```Haskell
import System.Random

main = do
  nums <- take 3 . randomRs (1, 10) <$> getStdGen :: IO [Int]
  putStrLn $ "랜덤한 숫자들: " ++ show nums
```

출력 예시:

```
랜덤한 숫자들: [4, 7, 1]
```

## 딥 다이브

Haskell에서 랜덤한 숫자를 생성하는 것은 실제로 매우 복잡하지 않습니다. 이는 Haskell의 Lazy Evaluation 방식으로 인해 가능합니다. "randomRIO" 함수는 IO 모나드와 함께 작동하기 때문에 순수 함수형 프로그래밍 언어인 Haskell에서 랜덤한 숫자를 생성하는 것이 가능합니다.

또한 랜덤한 숫자를 사용하는 알고리즘에 따라 랜덤성의 수준이 다를 수 있습니다. 따라서 정말 완벽한 랜덤성을 원한다면 랜덤 씨드를 직접 제공하는 방법이 아닌 다른 방식으로 랜덤한 숫자를 생성하는 것이 좋습니다.

## 같이보기

- [Haskell에서 난수 만들기](https://www.haskell.org/haskellwiki/Random_number_generation)
- [Haskell에서 재미있는 게임 만들기](https://medium.com/@wee3/haskell-tutorial-3-writing-a-simple-game-thing-9e4b94615d7b)
- [Haskell의 랜덤 씨드 및 시뮬레이션](http://member.nutn.edu.tw/~leeht/FPL13/5-monads/SPJ%20Monads.pdf)