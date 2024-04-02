---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:21.815097-07:00
description: "\uC5F0\uAD00 \uBC30\uC5F4\uC740 Swift\uC5D0\uC11C\uB294 \uC0AC\uC804\
  (Dictionary)\uC774\uB77C\uACE0 \uC54C\uB824\uC838 \uC788\uC73C\uBA70, \uD0A4-\uAC12\
  \ \uC30D\uC73C\uB85C \uB370\uC774\uD130\uB97C \uC800\uC7A5\uD558\uACE0 \uAD00\uB9AC\
  \uD560 \uC218 \uC788\uAC8C \uD574\uC90D\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uACE0\uC720\uD55C \uD0A4\uB97C \uAE30\uBC18\uC73C\uB85C \uAC12\uC744\
  \ \uC27D\uAC8C \uC811\uADFC\uD558\uACE0 \uC870\uC791\uD558\uAE30 \uC704\uD574 \uC774\
  \uB97C \uB370\uC774\uD130\uB97C \uD6A8\uC728\uC801\uC73C\uB85C \uAD6C\uC131\uD558\
  \uB294 \uB370 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.721534-06:00'
model: gpt-4-0125-preview
summary: "\uC5F0\uAD00 \uBC30\uC5F4\uC740 Swift\uC5D0\uC11C\uB294 \uC0AC\uC804(Dictionary)\uC774\
  \uB77C\uACE0 \uC54C\uB824\uC838 \uC788\uC73C\uBA70, \uD0A4-\uAC12 \uC30D\uC73C\uB85C\
  \ \uB370\uC774\uD130\uB97C \uC800\uC7A5\uD558\uACE0 \uAD00\uB9AC\uD560 \uC218 \uC788\
  \uAC8C \uD574\uC90D\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uACE0\
  \uC720\uD55C \uD0A4\uB97C \uAE30\uBC18\uC73C\uB85C \uAC12\uC744 \uC27D\uAC8C \uC811\
  \uADFC\uD558\uACE0 \uC870\uC791\uD558\uAE30 \uC704\uD574 \uC774\uB97C \uB370\uC774\
  \uD130\uB97C \uD6A8\uC728\uC801\uC73C\uB85C \uAD6C\uC131\uD558\uB294 \uB370 \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

## 무엇 & 왜?

연관 배열은 Swift에서는 사전(Dictionary)이라고 알려져 있으며, 키-값 쌍으로 데이터를 저장하고 관리할 수 있게 해줍니다. 프로그래머들은 고유한 키를 기반으로 값을 쉽게 접근하고 조작하기 위해 이를 데이터를 효율적으로 구성하는 데 사용합니다.

## 어떻게:

Swift는 연관 배열을 다루기 쉽게 만들어줍니다. 다음은 Swift 사전에서 항목을 선언하고, 추가하고, 제거하고, 접근하는 방법입니다:

```Swift
// 사전 선언
var fruitColors: [String: String] = ["Apple": "Red", "Banana": "Yellow"]

// 새 항목 추가
fruitColors["Grape"] = "Purple"

// 키를 사용하여 값에 접근
if let appleColor = fruitColors["Apple"] {
    print("Apple은 \(appleColor)입니다.")  // 출력: Apple은 Red입니다.
} else {
    print("색상을 찾을 수 없습니다.")
}

// 항목 제거
fruitColors["Banana"] = nil  // 이것은 사전에서 "Banana"를 제거할 것입니다

// 항목 순회
for (fruit, color) in fruitColors {
    print("\(fruit)은 \(color)입니다.")
    // 출력:
    // Apple은 Red입니다.
    // Grape은 Purple입니다.
}
```

사전은 굉장히 다재다능하며, 데이터를 동적으로 조작하고 접근할 수 있게 해줍니다. 데이터 검색 속도에 영향을 주지 않는 그들의 순서 없는 특성은 큰 데이터 세트를 다룰 때 중요한 이점입니다.

## 심층 탐구

Swift에서 사전을 연관 배열로 구현하는 것은 고유한 키를 값에 매핑하는 그들의 강력한 능력에서 기인합니다. 역사적으로, 프로그래밍 언어는 이 개념을 해시 테이블이나 맵과 같은 다양한 이름으로 구현해왔으며, 이는 키와 값 사이에 "맵"을 생성하는 기능을 암시합니다.

Swift에서 사전은 성능을 위해 최적화되어 있으며, 효율적인 데이터 검색을 위해 해시 가능한 키를 활용합니다. 이는 `[Key: Value]` 사전에서 `Key` 타입이 `Hashable` 프로토콜을 준수해야 함을 의미하는데, `Int`, `String`, `Double`과 같은 대부분의 Swift 표준 타입들에 대해서는 해당사항이 있습니다.

한 가지 고려할 사항은, 사전이 데이터 쌍을 연관짓기에는 뛰어나지만 순서가 없다는 것입니다. 요소의 순서를 유지해야 하는 경우, 순서가 지정된 요소의 시퀀스를 위한 배열(Array) 또는 배열과 사전의 기능을 결합한 사용자 정의 데이터 구조를 탐색해볼 수 있습니다.

또한 Swift는 지속적으로 발전하고 있으며, 사전의 처리 및 최적화 방법도 진화하고 있습니다. 따라서 사전을 최대한 활용하고 최신 및 가장 효율적인 실천 방법을 사용하려면 최신 Swift 문서를 계속해서 업데이트하는 것이 중요합니다.
