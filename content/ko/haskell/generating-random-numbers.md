---
title:                "Haskell: 랜덤 숫자 생성"
programming_language: "Haskell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜?

난수를 생성하는 것에 참여하는 이유는 우리가 실제 세계에서 발견할 수 있는 다양한 현상을 모델링하고 문제를 해결하기 위해서 입니다. 이러한 현상과 문제는 랜덤한 요소가 포함되어 있기 때문에 랜덤한 숫자를 생성하는 것이 중요합니다.

## 방법

랜덤한 숫자를 생성하기 위해서 우리는 `System.Random` 모듈을 사용할 수 있습니다. 이 모듈은 `random`, `randomR`, `randomRs`와 같은 유용한 함수를 제공합니다. 예를 들어, 1부터 10 사이의 랜덤한 정수를 생성하고 싶다면 다음과 같이 할 수 있습니다:

``` Haskell
randomNumber :: IO Int
randomNumber = do
  gen <- newStdGen -- 새로운 랜덤 생성기를 생성합니다.
  let (num, _) = randomR (1, 10) gen -- 1부터 10 사이의 랜덤한 정수를 생성합니다.
  return num
```

위의 코드를 실행하면 매번 다른 랜덤한 숫자가 출력될 것입니다. 이제 우리는 이러한 함수를 사용하여 여러분의 프로그램에 랜덤한 요소를 추가할 수 있습니다.

## 더 깊게 들어가기

랜덤한 숫자를 생성하는 방법에는 여러 가지가 있습니다. 예를 들어, 날짜와 시간을 기반으로 랜덤한 숫자를 생성할 수도 있습니다. 또한 시드 값을 변경하여 다른 랜덤 수열을 생성하는 것도 가능합니다.

또한 주의해야 할 점은 랜덤한 숫자는 항상 예측할 수 없다는 것입니다. 랜덤한 이벤트는 언제나 발생할 수 있기 때문에 예외 처리를 해주는 것이 좋습니다.

## 또 다른 방법 알아보기

- [HaskellWiki: Random](https://wiki.haskell.org/Random)
- [Real World Haskell: Randomness](http://book.realworldhaskell.org/read/randomness.html)
- [Haskell for all: How to generate random numbers](http://www.haskellforall.com/2016/04/how-to-generate-random-numbers-in-haskell.html)

## 참고

- [Markdown Syntax Guide](https://www.markdownguide.org/basic-syntax/)
- [System.Random 모듈 문서](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html)