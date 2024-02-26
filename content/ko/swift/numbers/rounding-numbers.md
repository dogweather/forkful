---
date: 2024-01-26 03:46:53.400156-07:00
description: "\uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD55C\uB2E4\uB294 \uAC83\uC740\
  \ \uC77C\uBC18\uC801\uC73C\uB85C \uC6D0\uCE58 \uC54A\uB294 \uC18C\uC218\uC810\uC744\
  \ \uC5C6\uC560\uAE30 \uC704\uD574 \uC218\uCE58 \uAC12\uC744 \uD2B9\uC815 \uC815\uBC00\
  \uB3C4\uB85C \uADFC\uC0AC\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBA54\uBAA8\uB9AC \uAD00\uB9AC, \uAC00\
  \uB3C5\uC131 \uD5A5\uC0C1 \uBC0F \uD654\uD3D0 \uC81C\uC57D\uACFC \uAC19\uC740 \uB3C4\
  \uBA54\uC778 \uD2B9\uC815 \uC694\uAD6C\uC0AC\uD56D\uC744 \uCDA9\uC871\uD558\uAE30\
  \ \uC704\uD574 \uBC18\uC62C\uB9BC\uC744 \uD569\uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:52.705866-07:00'
model: gpt-4-0125-preview
summary: "\uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD55C\uB2E4\uB294 \uAC83\uC740 \uC77C\
  \uBC18\uC801\uC73C\uB85C \uC6D0\uCE58 \uC54A\uB294 \uC18C\uC218\uC810\uC744 \uC5C6\
  \uC560\uAE30 \uC704\uD574 \uC218\uCE58 \uAC12\uC744 \uD2B9\uC815 \uC815\uBC00\uB3C4\
  \uB85C \uADFC\uC0AC\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBA54\uBAA8\uB9AC \uAD00\uB9AC, \uAC00\uB3C5\
  \uC131 \uD5A5\uC0C1 \uBC0F \uD654\uD3D0 \uC81C\uC57D\uACFC \uAC19\uC740 \uB3C4\uBA54\
  \uC778 \uD2B9\uC815 \uC694\uAD6C\uC0AC\uD56D\uC744 \uCDA9\uC871\uD558\uAE30 \uC704\
  \uD574 \uBC18\uC62C\uB9BC\uC744 \uD569\uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
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
