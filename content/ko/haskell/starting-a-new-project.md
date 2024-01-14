---
title:                "Haskell: 새 프로젝트 시작하기"
programming_language: "Haskell"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

# 왜

새로운 프로젝트를 시작하는 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 새로운 언어나 프레임워크를 배우고 싶거나, 기존의 문제를 더 나은 방식으로 해결하고 싶을 수도 있습니다. 또는 더 효율적인 코드를 작성하고 싶거나, 개인적인 만족감을 얻기 위해서일 수도 있습니다.

# 시작하기

Haskell은 함수형 프로그래밍 언어로, 다른 언어와는 달리 조금 독특한 문법을 가지고 있습니다. 하지만 한 번 익숙해지고 나면 매우 강력한 언어라는 것을 알게 될 것입니다. 여기서는 간단한 예제를 통해 Haskell의 기본 문법을 살펴보고, 새로운 프로젝트를 시작하는 방법을 알아보겠습니다.

```Haskell
-- "Hello, world!"를 출력합니다.
main = putStrLn "Hello, world!"

-- 숫자를 입력받아 제곱을 계산하는 함수입니다.
square :: Int -> Int
square x = x * x

-- 위에서 정의한 square 함수를 사용하여 5의 제곱을 계산합니다.
-- 출력 결과는 25가 됩니다.
main = print (square 5)
```

# 깊게 들어가기

새로운 프로젝트를 시작할 때 가장 중요한 것은 처음부터 제대로 계획하는 것입니다. 그렇기 때문에 인터넷에서 찾은 다양한 자료들을 참고하고, 다른 프로그래밍 언어를 배울 때처럼 기초적인 것부터 차근차근 학습하는 것이 좋습니다. 또한 레퍼런스 문서를 자주 참고하고, 지속적으로 코드를 수정하며 발전시켜 나가는 것이 중요합니다.

# 관련 자료

- [Haskell Wiki](https://wiki.haskell.org)
- [Real World Haskell (한국어 번역판)](https://github.com/smilejay/real-world-haskell-ko)
- [Learn You a Haskell for Great Good](http://learnyouahaskell.com)
- [Hackage: Haskell 패키지 저장소](https://hackage.haskell.org)