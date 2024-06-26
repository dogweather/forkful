---
date: 2024-01-27 20:35:33.167862-07:00
description: "\uBC29\uBC95: Swift\uB294 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB97C \uD1B5\uD574 \uBB34\uC791\uC704 \uC22B\uC790\uB97C \uC0DD\uC131\uD558\uB294\
  \ \uAC04\uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uB2E4\uC74C\
  \uC740 \uB2E4\uC591\uD55C \uC22B\uC790 \uC720\uD615\uC5D0 \uB300\uD574 \uC774\uB97C\
  \ \uC218\uD589\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.725992-06:00'
model: gpt-4-0125-preview
summary: "Swift\uB294 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD1B5\uD574\
  \ \uBB34\uC791\uC704 \uC22B\uC790\uB97C \uC0DD\uC131\uD558\uB294 \uAC04\uB2E8\uD55C\
  \ \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uB09C\uC218 \uC0DD\uC131"
weight: 12
---

## 방법:
Swift는 표준 라이브러리를 통해 무작위 숫자를 생성하는 간단한 방법을 제공합니다. 다음은 다양한 숫자 유형에 대해 이를 수행하는 방법입니다:

```Swift
// 0과 Int.max 사이의 무작위 정수 생성
let randomInt = Int.random(in: 0...Int.max)
print(randomInt)

// 0.0과 1.0 사이의 무작위 부동 소수점 숫자 생성
let randomDouble = Double.random(in: 0.0...1.0)
print(randomDouble)

// 무작위 Bool 값 생성
let randomBool = Bool.random()
print(randomBool)
```

샘플 출력은 변할 수 있습니다. 왜냐하면 어쨌든 우리는 무작위성을 다루고 있기 때문입니다. 코드를 여러 번 실행하면 다른 숫자와 불리언 값이 생성됩니다.

## 심화 탐구
Swift의 무작위 숫자 생성 접근 방식은 견고하고 효율적인 의사 난수 생성기(PRNG) 위에 구축되어 있습니다. Swift 4.2 이전에는 개발자들이 외부 라이브러리나 기본 플랫폼 기능에 의존하여, 다양한 플랫폼과 환경에서 일관성이 없을 수 있었습니다. Swift 4.2에서 네이티브 API 도입으로 무작위 숫자를 생성하는 것이 더 간단해지고 기본 플랫폼에 관계없이 더 일관되게 되었습니다.

그러나 Swift의 표준 무작위 숫자 생성기는 암호화 목적에는 적합하지 않다는 것을 이해하는 것이 중요합니다. 암호화를 위해 개발자들은 애플 플랫폼에서 `Security` 프레임워크를 사용해야 합니다. 이는 암호화에 안전한 무작위 바이트에 액세스할 수 있게 합니다. 제 마지막 업데이트 시점에서, Swift는 그것의 표준 라이브러리에 플랫폼 간 암호화 무작위 숫자 생성기를 포함하고 있지 않으며, 비애플 플랫폼에서 이러한 요구를 충족하기 위해 개발자들은 제3자 라이브러리를 찾아야 합니다.

과학적 컴퓨팅 영역이나 의사 무작위 숫자의 결정적 시퀀스(시퀀스를 정확하게 재생산할 수 있음)가 필요한 상황에서는, 생성기를 시드할 수 있는 기능 없이 Swift의 무작위 숫자 생성이 가장 적합하지 않을 수 있습니다. 이러한 경우, 이러한 정확한 요구 사항을 충족하기 위해 전문 라이브러리와 알고리즘이 종종 사용됩니다.
