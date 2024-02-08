---
title:                "숫자 반올림하기"
date:                  2024-01-26T03:45:37.386168-07:00
model:                 gpt-4-0125-preview
simple_title:         "숫자 반올림하기"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/rounding-numbers.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 그런가?

숫자를 반올림하는 것은 그것들을 가장 가까운 정수로 조정하거나 지정된 정밀도로 조정하는 것을 말합니다. 프로그래머들은 가독성을 향상시키기 위해, 저장 요구 사항을 줄이기 위해, 또는 정확한 값이 후속 계산에 결정적이지 않기 때문에 이를 수행합니다.

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
