---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

랜덤 숫자 생성은 예측할 수 없는 숫자를 만드는 프로그래밍 기법입니다. 이를 통해 프로그래머는 유저입력 없이 다양성과 무작위성을 코딩 내에 주입할 수 있습니다.

## 사용 방법:

Haskell에서는 System.Random 모듈을 사용하여 쉽게 랜덤 숫자를 생성할 수 있습니다. 아래는 간단한 랜덤 숫자 생성 코드입니다.

```Haskell
import System.Random

main :: IO()
main = do
  g <- newStdGen
  print (take 5 (randoms g :: [Int]))
```

이 코드는 표준 제너레이터를 이용하여 5개의 랜덤한 정수를 출력합니다.

## 깊이 들어가기

이번 섹션에서는 랜덤 숫자 생성의 역사적 배경, 대안 그리고 구현 상세에 대해 더 깊게 알아보겠습니다.

1. 역사적 배경: 초기 컴퓨터 시스템에서는 랜덤 숫자 생성기가 부재했으며, 대신 고정된 테이블을 사용하여 이를 대체하곤했습니다. 하지만 시간이 지남에 따라 여러 알고리즘이 개발되어 현재에 이르게 되었습니다.

2. 대안: System.Random 외에도 다른 Haskell 라이브러리들이 랜덤 숫자 생성을 지원합니다. 예를 들어, 'random-fu' 패키지는 확률 분포에 따른 랜덤 생성을 지원합니다.

3. 구현 상세: Haskell의 `newStdGen` 함수는 시스템의 현재 시간에 기반하여 새로운 랜덤 숫자 생성기를 생성합니다. 이후 나온 랜덤 숫자들은 이전에 생성된 숫자에 의존하는 pseudo-random algorithm에 따라 생성됩니다.

## 참고 자료

Haskell과 랜덤 숫자 생성에 관한 추가 정보를 원하면 아래 링크를 참조하세요.
1. [Random Numbers: Introduction - School of Haskell | FP Complete](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms)
2. [System.Random – Hackage](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html)