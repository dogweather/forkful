---
date: 2024-01-26 03:45:37.386168-07:00
description: "\uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD558\uB294 \uAC83\uC740 \uADF8\
  \uAC83\uB4E4\uC744 \uAC00\uC7A5 \uAC00\uAE4C\uC6B4 \uC815\uC218\uB85C \uC870\uC815\
  \uD558\uAC70\uB098 \uC9C0\uC815\uB41C \uC815\uBC00\uB3C4\uB85C \uC870\uC815\uD558\
  \uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uAC00\uB3C5\uC131\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uAE30 \uC704\uD574, \uC800\
  \uC7A5 \uC694\uAD6C \uC0AC\uD56D\uC744 \uC904\uC774\uAE30 \uC704\uD574, \uB610\uB294\
  \ \uC815\uD655\uD55C \uAC12\uC774 \uD6C4\uC18D \uACC4\uC0B0\uC5D0 \uACB0\uC815\uC801\
  \uC774\uC9C0 \uC54A\uAE30 \uB54C\uBB38\uC5D0 \uC774\uB97C \uC218\uD589\uD569\uB2C8\
  \uB2E4."
lastmod: '2024-03-13T22:44:55.164130-06:00'
model: gpt-4-0125-preview
summary: "\uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD558\uB294 \uAC83\uC740 \uADF8\uAC83\
  \uB4E4\uC744 \uAC00\uC7A5 \uAC00\uAE4C\uC6B4 \uC815\uC218\uB85C \uC870\uC815\uD558\
  \uAC70\uB098 \uC9C0\uC815\uB41C \uC815\uBC00\uB3C4\uB85C \uC870\uC815\uD558\uB294\
  \ \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
weight: 13
---

## 방법:
Kotlin에서는 `roundToInt()`, `roundToDouble()` 및 더 많은 제어를 위해 `BigDecimal`을 사용하는 여러 함수를 사용하여 반올림을 할 수 있습니다:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // 출력: 3

    val number2 = 3.5
    println(number2.roundToInt()) // 출력: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // 출력: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // 출력: 123.5
}
```

## 깊이 있는 탐구
역사적으로, 숫자를 반올림하는 것은 수학과 계산에서 수치 정밀도의 한계를 다루도록 설계된 기본 개념이었습니다. 초기 컴퓨팅에서는 메모리의 높은 비용 때문에 반올림이 중요했습니다.

Kotlin에서 반올림은 표준 Java 라이브러리를 기반으로 합니다. 반올림을 위한 옵션으로는 가장 가까운 정수로 반올림하는 `Math.round()`와, 스케일과 `RoundingMode`를 지정할 수 있는 사용자 지정 반올림이 가능한 `BigDecimal`이 있습니다.

각 `RoundingMode`는 반올림할 옵션의 가운데에 정확히 위치한 숫자(즉, 동일한 거리에 있는 두 이웃 사이)를 다루기 위한 다른 정책을 가지고 있습니다. 예를 들어, `RoundingMode.HALF_UP`은 가장 가까운 이웃으로 반올림하되, 이웃이 동일한 거리에 있을 경우에는 올림합니다.

## 참고
- Kotlin 문서에서 [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- Oracle의 자바 문서에서 [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- 부동 소수점 산술에 대한 IEEE 표준 (IEEE 754) [IEEE 표준 754](https://ieeexplore.ieee.org/document/4610935)
