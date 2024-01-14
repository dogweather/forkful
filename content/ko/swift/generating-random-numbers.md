---
title:                "Swift: 랜덤 숫자 생성하기"
programming_language: "Swift"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜
우리는 프로그래밍을 통해 무작위 수를 생성하는 것을 알아볼 것입니다. 이렇게 함으로써 우리는 컴퓨터 프로그램의 다양한 기능을 사용할 수 있으며, 재미있는 결과를 얻을 수 있습니다.

## 해는
프로그래밍에서 무작위 수를 생성하는 방법은 다양합니다. 예를 들어, 그들은 배열, 문자열, 또는 특정 범위 내에서 값을 얻을 수 있습니다. 이것들을 만드는 가장 간단한 방법 중 하나는 Swift의 `arc4random()` 함수를 사용하는 것입니다.

```Swift
// 0과 100 사이의 무작위 정수 생성
let randomNumber = arc4random_uniform(101)
print(randomNumber)
// Output: 50, 73, 25, 등등...
```

반복문과 조건문을 사용하여 여러 개의 무작위 수를 생성하거나, `arc4random()` 함수를 이용하여 특정 범위 내에서 무작위 수를 생성할 수도 있습니다.

## 딥 다이브
무작위 수를 생성하는 것은 우리가 쉽게 할 수 있는 것처럼 보일 수 있지만, 컴퓨터 프로그래밍에서는 매우 중요합니다. 그 이유는 우리의 프로그램을 더 다양하고 재미있게 만들 수 있기 때문입니다. 또한, 무작위 수를 생성하는 알고리즘을 이해하면 다른 문제를 해결하는 데에도 도움이 됩니다.

## 참고 자료
- [Swift Documentation: Random Numbers](https://docs.swift.org/swift-book/LanguageGuide/Functions.html#ID167)
- [Swift Tutorial: How to Generate Random Numbers](https://www.hackingwithswift.com/example-code/language/how-to-generate-random-numbers-using-swift)
- [Examples of Random Functions in Swift](https://www.freecodecamp.org/news/the-complete-guide-to-random-functions-in-swift-5b55a1b65da9/)
- [Random Number Generation in Programming](https://en.wikipedia.org/wiki/Random_number_generation)