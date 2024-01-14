---
title:    "Swift: 난수 생성"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜
난수를 생성하는 것에 참여하는 이유를 간략히 설명합니다.

## 하는 방법
난수를 생성하는 다양한 방법과 코드 예제를 ```Swift ... ``` 코드 블록을 사용하여 보여줍니다. 실제 출력 결과도 확인할 수 있습니다.

```Swift
// Int 타입의 난수 생성
let randomInt = Int.random(in: 1...10)
print(randomInt) // 출력: 8

// 배열에서 랜덤하게 값 추출
let fruits = ["apple", "banana", "orange"]
let randomFruit = fruits.randomElement()
print(randomFruit) // 출력: apple
```

## 깊게 파헤치기
난수 생성의 깊은 이해를 위한 추가 정보를 제공합니다. 난수를 생성하는 알고리즘과 난수 발생기의 원리에 대해 알아보고, 난수 생성의 중요성과 활용 방법도 살펴봅니다.

## 또 다른 정보
이 글을 읽은 후에 더 자세히 알아보실 수 있는 링크 목록입니다.

[난수 생성 관련 Swift 문서](https://developer.apple.com/documentation/swift/double/3126623-random)

[난수 생성 알고리즘 설명 및 예제](https://www.geeksforgeeks.org/random-numbers-in-swift/)

[난수 발생기의 작동 원리에 대한 이해](https://medium.com/@andreasliefooghe/how-satisfies-uniform-random-a-swift-implementation-cae04dfab509)