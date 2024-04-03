---
date: 2024-01-26 03:46:53.400156-07:00
description: "\uC5B4\uB5BB\uAC8C: Swift\uC5D0\uB294 \uC22B\uC790\uB97C \uBC18\uC62C\
  \uB9BC\uD558\uB294 \uBA87 \uAC00\uC9C0 \uBC29\uBC95\uC774 \uC788\uC2B5\uB2C8\uB2E4\
  . \uC77C\uBD80\uB97C \uC18C\uAC1C\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.724361-06:00'
model: gpt-4-0125-preview
summary: "Swift\uC5D0\uB294 \uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD558\uB294 \uBA87\
  \ \uAC00\uC9C0 \uBC29\uBC95\uC774 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
weight: 13
---

## 어떻게:
Swift에는 숫자를 반올림하는 몇 가지 방법이 있습니다. 일부를 소개합니다:

```Swift
let original = 3.14159

// 표준 반올림
let standardRounded = round(original) // 3.0

// 특정 소수점까지 반올림
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// 내림
let roundedDown = floor(original) // 3.0

// 올림
let roundedUp = ceil(original) // 4.0

print("표준: \(standardRounded), 소수점: \(decimalRounded), 내림: \(roundedDown), 올림: \(roundedUp)")
```

출력: `표준: 3.0, 소수점: 3.142, 내림: 3.0, 올림: 4.0`

## 깊이 있는 분석
역사적으로, 반올림은 컴퓨터 이전에도 있었던 수학적 개념으로, 상업과 과학에서 필수적입니다. Swift의 `Foundation` 프레임워크는 포괄적인 반올림 기능을 제공합니다:

- `round(_: )`은 좋은 옛날 반올림입니다.
- `floor(_: )`와 `ceil(_: )`은 방향 반올림을 처리합니다.
- `rounded(.up/.down/.toNearestOrAwayFromZero)`는 반올림 규칙 enum으로 더 세밀한 제어를 가능하게 합니다.

정밀한 금융 계산을 위한 `Decimal` 타입을 주의해야 하며, 이는 부동 소수점 오류를 피할 수 있습니다. 또한 Objective-C 호환성을 위해 `NSDecimalNumber`를 탐색하세요.

## 참조
- IEEE 부동 소수점 산술 표준 (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
