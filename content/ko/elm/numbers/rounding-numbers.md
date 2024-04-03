---
date: 2024-01-26 03:44:30.482558-07:00
description: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uC740 \uC18C\uC218\uC810\uC744 \uAC00\
  \uC7A5 \uAC00\uAE4C\uC6B4 \uC815\uC218\uB85C \uC870\uC815\uD558\uAC70\uB098 \uC9C0\
  \uC815\uB41C \uC18C\uC218\uC810 \uC790\uB9BF\uC218\uB85C \uBC18\uC62C\uB9BC\uD558\
  \uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB294 \uBCF5\uC7A1\uC131\uC744 \uC904\uC774\uACE0, \uAC00\uB3C5\uC131\uC744 \uD5A5\
  \uC0C1\uC2DC\uD0A4\uAC70\uB098, \uC815\uBC00\uB3C4 \uC694\uAD6C \uC0AC\uD56D\uC744\
  \ \uCDA9\uC871\uC2DC\uD0A4\uAE30 \uC704\uD574 \uBC18\uC62C\uB9BC\uD569\uB2C8\uB2E4\
  ."
lastmod: '2024-03-13T22:44:55.103286-06:00'
model: gpt-4-0125-preview
summary: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uC740 \uC18C\uC218\uC810\uC744 \uAC00\uC7A5\
  \ \uAC00\uAE4C\uC6B4 \uC815\uC218\uB85C \uC870\uC815\uD558\uAC70\uB098 \uC9C0\uC815\
  \uB41C \uC18C\uC218\uC810 \uC790\uB9BF\uC218\uB85C \uBC18\uC62C\uB9BC\uD558\uB294\
  \ \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
weight: 13
---

## 무엇과 왜?

숫자 반올림은 소수점을 가장 가까운 정수로 조정하거나 지정된 소수점 자릿수로 반올림하는 것을 의미합니다. 프로그래머는 복잡성을 줄이고, 가독성을 향상시키거나, 정밀도 요구 사항을 충족시키기 위해 반올림합니다.

## 사용 방법:

Elm의 `Basics` 모듈은 반올림을 위한 멋진 함수들을 제공합니다: `round`, `floor`, `ceiling`. 사용 방법은 다음과 같습니다.

```elm
import Basics exposing (round, floor, ceiling)

-- 가장 가까운 정수로 반올림
round 3.14    --> 3
round 3.5     --> 4

-- 내림
floor 3.999   --> 3

-- 올림
ceiling 3.001 --> 4

-- 소수점 제거
truncate 3.76 --> 3
```

Elm은 고정된 소수점 자릿수로 반올림하는 `toLocaleString`도 제공합니다:

```elm
import Float exposing (toLocaleString)

-- 소수점 두 자리로 반올림
toLocaleString 2 3.14159 --> "3.14"
```

## 심층 분석

Elm은 부수 효과(side effects)를 아키텍처의 "가장자리"로 배치하는, 강타입(strongly typed) 함수형 언어입니다. 이는 반올림과 같은 함수들이 순수하고 예측 가능해야 함을 의미합니다. 역사적으로 반올림은 부동소수점 연산의 부정확성을 다루는 많은 프로그래밍 언어에서 흔히 이루어지는 연산입니다.

Elm의 반올림 접근 방식은 직관적입니다 - 함수들은 순수하며 반올림, 내림, 올림에 대한 수학적 정의를 따릅니다. Elm은 내장 함수를 제공함으로써 특히 재무 및 그래픽 분야에서 자주 요구되는 정밀도 관리와 같은 일반적인 요구 사항을 예상합니다.

Elm의 내장 함수에 대한 대안은 산술 연산을 사용하는 사용자 정의 구현을 포함할 수 있지만, 표준 라이브러리가 이미 효율적으로 작업을 수행할 때 불필요한 복잡성을 추가합니다.

현재 버전에서 Elm은 이러한 연산을 위해 JavaScript의 기본 부동소수점 수학을 사용하므로, IEEE 754 표준과 일관성을 유지하며, 정밀도와 잠재적인 부동소수점 오류를 고려할 때 이 점을 기억하는 것이 중요합니다.

## 참고 자료

- Elm 공식 `Basics` 모듈 문서: https://package.elm-lang.org/packages/elm/core/latest/Basics
- 컴퓨팅에서 부동소수점 숫자 작동 방식에 대한 자세한 설명: https://floating-point-gui.de/
- 더 많은 부동소수점 연산을 위한 Elm `Float` 모듈: https://package.elm-lang.org/packages/elm/core/latest/Float
