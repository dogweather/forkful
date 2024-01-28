---
title:                "숫자 반올림하기"
date:                  2024-01-26T03:46:53.400156-07:00
model:                 gpt-4-0125-preview
simple_title:         "숫자 반올림하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/rounding-numbers.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜?

숫자를 반올림한다는 것은 일반적으로 원치 않는 소수점을 없애기 위해 수치 값을 특정 정밀도로 근사하는 것을 의미합니다. 프로그래머들은 메모리 관리, 가독성 향상 및 화폐 제약과 같은 도메인 특정 요구사항을 충족하기 위해 반올림을 합니다.

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
