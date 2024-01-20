---
title:                "임의의 숫자 생성하기"
html_title:           "Elixir: 임의의 숫자 생성하기"
simple_title:         "임의의 숫자 생성하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
무작위 숫자 생성은 예상할 수 없는 숫자를 생성하는 것을 의미합니다. 이는 게임, 시뮬레이션, 머신 러닝 등에서 불규칙성을 제공하거나 데이터를 암호화하는 데 필요합니다.

## 사용 방법:
Swift에서 무작위 정수를 생성하는 가장 간단한 방법입니다.
```swift
import Swift

// 0부터 10까지의 무작위 정수 생성
let randomInt = Int.random(in: 0..<10)
print(randomInt)
```
출력: `5`

Swift는 Float와 Double에 대해서도 비슷한 함수를 제공합니다.
```swift
// 0부터 1 사이의 무작위 Float 생성
let randomFloat = Float.random(in: 0..<1)
print(randomFloat)
// 0.723614

let randomDouble = Double.random(in: 0..<1)
print(randomDouble)
// 0.287488
```
## 깊게 알아보기:

무작위 숫자를 생성하는 것은 컴퓨터 과학의 오래된 문제입니다. 예전에는 난수표나 특정 알고리즘을 사용하여 '난수'를 만들었습니다. 그러나 이 방법들은 항상 동일한 숫자를 생성하므로 정말로 '무작위'라고 할 수 없었습니다.

Swift는 암호학적으로 안전한 난수 발생기를 내장하여 이 문제를 해결합니다. 이는 임의의 정수, 부동 소수점 수, Bool 값을 생성할 수 있습니다. 이외에도, Swift의 `SystemRandomNumberGenerator`는 성능을 최적화 하여 사용할 수 있습니다.

다른 언어나 프레임워크에서도 유사한 기능을 제공합니다. Python의 `random` 모듈이나 JavaScript의 `Math.random()` 함수 등이 그 예입니다.

## 참고 문헌:
[Swift Standard Library Documentation: Generating Random Numbers](https://developer.apple.com/documentation/swift/swift_standard_library/random_numbers)

[Swift.org - Random Unification Proposal](https://github.com/apple/swift-evolution/blob/master/proposals/0202-random-unification.md)

[Swift Random Source Code](https://github.com/apple/swift/blob/master/stdlib/public/Darwin/Darwin.swift)