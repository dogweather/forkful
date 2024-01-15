---
title:                "랜덤 숫자 생성하기"
html_title:           "Haskell: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

컴퓨터 프로그래밍을 하다보면 종종 난수(random numbers)를 생성해야 하는 일이 생기는데, 이는 게임 개발, 시뮬레이션, 암호화 등 다양한 분야에서 유용합니다.

## 방법

```Haskell
-- 난수 생성을 위해서는 System.Random 모듈을 불러와야 합니다
import System.Random

-- 1부터 10까지의 난수 생성
generateRandomNumber :: IO Int
generateRandomNumber = randomRIO (1, 10)

-- 난수가 나오는 예제 실행
main :: IO ()
main = do
  randomNumber <- generateRandomNumber
  putStrLn $ "오늘의 운세는 " ++ show randomNumber ++ "입니다!"
```
출력 예시:

`오늘의 운세는 7입니다!`

## 깊게 파보기

난수를 생성하는 방법은 다양하지만, Haskell에서는 `random`과 `randomRIO` 함수를 사용하여 간단하게 난수를 생성할 수 있습니다. `random`함수는 임의의 난수를 생성할 뿐만 아니라 생성되는 난수의 타입 또한 지정할 수 있습니다. 또한 `randomRIO` 함수는 특정 범위 안에서 난수를 생성할 수 있습니다.

## 더 보기

- [Haskell Random 모듈 문서](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)