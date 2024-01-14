---
title:                "Swift: 난수 생성하기"
simple_title:         "난수 생성하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 왜 랜덤 숫자 생성에 참여해야 하는가?

일상생활에서 우리는 다양한 상황에서 랜덤한 숫자가 필요할 때가 있습니다. 예를 들어, 게임에서 랜덤으로 적이 나타날 때, 추첨 이벤트에서 당첨 번호를 뽑을 때 등 다양한 상황에서 랜덤 숫자 생성이 필요합니다.

## 어떻게 하나요?

Swift는 랜덤 숫자를 생성하는데 유용한 `arc4random()` 함수를 제공합니다. 이 함수는 0부터 입력한 정수 사이의 랜덤한 숫자를 반환합니다. 아래의 예제 코드를 참고해보세요.

```Swift
let randomNum = arc4random_uniform(10)
print(randomNum)
```

위 코드는 0부터 10 사이의 랜덤한 숫자를 생성하고 출력하는 예제입니다. 실행해보면 매번 다른 숫자가 출력되는 것을 확인할 수 있습니다.

## 깊이 파고들어보기

`arc4random()` 함수는 랜덤 숫자를 생성하는 방법 중 하나일 뿐만 아니라 정확한 랜덤성을 보장하지 않습니다. 만약 더 강력한 랜덤성이 필요하다면 `arc4random_uniform()` 대신 `random()` 함수를 사용하는 것이 좋습니다. 또한, 시드(seed) 값을 이용해서 순서대로 같은 랜덤한 숫자를 생성할 수도 있습니다.

## 참고 자료

- [Swift 공식 문서 - 랜덤 숫자 생성](https://developer.apple.com/documentation/swift/fastfloat/random_number_generation)
- [iOS Academy - Generating Random Numbers in Swift](https://www.iosacademy.io/generating-random-numbers-in-swift/)
- [Swift School - How to Generate Random Numbers in Swift](https://swiftschool.io/random-number-generating-swift/)