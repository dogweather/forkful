---
title:                "랜덤 숫자 생성"
html_title:           "Haskell: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
(1) 랜덤한 숫자를 생성하는 것은 프로그래머가 무작위성을 필요로 하는 작업을 수행할 수 있게 해줍니다.
(2) 랜덤한 숫자를 생성하는 이유는 보안, 게임 또는 실험 등 다양한 목적을 위해서입니다.

## 해는 포함하는 방법:
```Haskell
import System.Random

-- 범위 내 랜덤 정수 생성 예제
randomInt :: Int -> Int -> IO Int
randomInt a b = randomRIO (a, b)

-- 1부터 10까지의 랜덤 정수 출력 예제
main = do
  num <- randomInt 1 10
  putStrLn ("랜덤 정수: " ++ show num)
```

```Haskell
-- 리스트에서 랜덤한 요소 선택 예제
randomElement :: [a] -> IO a
randomElement [] = error "빈 리스트"
randomElement xs = do
  idx <- randomInt 0 (length xs - 1)
  return (xs !! idx)

-- 리스트에서 랜덤한 요소 출력 예제
main = do
  let numList = [1, 2, 3, 4, 5]
  num <- randomElement numList
  putStrLn ("랜덤 요소: " ++ show num)
```

출력 결과:
```
랜덤 정수: 7
랜덤 요소: 3
```

## 깊게 들어가기:
(1) 랜덤한 숫자를 생성하는 기술은 1946년 미국의 수학자인 존 폰 노이만에 의해 제안되었습니다.
(2) 다른 프로그래밍 언어에서는 보통 의사난수 생성기(Pseudo-Random Number Generator)를 사용하지만, 해스켈은 더 정교한 진정한 난수 생성기(True Random Number Generator)를 제공합니다.
(3) 해스켈의 경우 System.Random 라이브러리를 사용하여 랜덤 숫자를 생성할 수 있습니다.

## 관련 자료:
- https://hackage.haskell.org/package/random 아프리카 가빙글 19세기 언어 기능  - 내 프로젝트 무엇처럼 만들어무어 `RND` 구 상승한다. 2020 02