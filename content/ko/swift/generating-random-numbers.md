---
title:    "Swift: 랜덤 숫자 생성하기"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 왜
무작위 숫자를 생성하는 것에 참여할 이유는 무엇일까요?
무작위 숫자는 게임, 보안 검사 및 다양한 테스트 작업 등 다양한 분야에서 유용하게 사용될 수 있으며, 기본적인 프로그래밍 개념을 이해하는 데 중요합니다.

## 방법
Swift에서 무작위 수를 생성하는 방법을 살펴보겠습니다. 아래 코드 블록에서는 Swift 5.5 버전에서 사용할 수 있는 새로운 random() 메서드를 사용하여 무작위 정수를 생성하는 방법을 보여줍니다.

```Swift
// 범위 내에서 무작위 정수 생성
let randomNumber = Int.random(in: 1...10)
print(randomNumber) //출력 예: 7

// 특정 범위 내에서 무작위 실수 생성
let randomDouble = Double.random(in: 0..<1)
print(randomDouble) //출력 예: 0.3725011900257102
```

위 예제에서는 random() 메서드를 사용하여 범위 내에서 무작위로 정수와 실수를 생성하는 방법을 보여주었습니다. 또한 개발자는 나온 결과를 적절히 활용하기 위해 해당 데이터 형식을 적절하게 선택할 수 있습니다.

## 깊이 파고들기
Swift에서 무작위 수를 생성하는 메서드는 대부분에 있어서 편리한 기능입니다. 그러나, 이 기능은 언어 및 알고리즘 때문에 시스템 시각적 측정에 영향을 미칠 수 있습니다. 실제로 몇 차례의 실험 결과를 보면, 난수 생성 작업에 따라 메모리 소비량이 증가한다는 것을 알 수 있었습니다. 따라서, 개발자들은 무작위 수를 생성하는 용도로만 사용하기보다는 가능하면 최소한으로 사용하는 것을 권장합니다.

## 관련 자료
* [Swift Programming Language](https://developer.apple.com/documentation/swift)
* [Swift Standard Library: Random Numbers](https://developer.apple.com/documentation/swift/random)
* [WWDC21: What's new in Swift](https://developer.apple.com/videos/play/wwdc2021/10148/)