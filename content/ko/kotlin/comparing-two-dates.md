---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

두 날짜를 비교한다는 것은 한 날짜가 다른 날짜보다 이후, 이전 또는 동일한지 확인하는 것입니다. 이는 스케줄링, 날짜 간 차이 계산 등 다양한 알고리즘에서 필수적인 부분입니다.

## 방법:

본 코드 블록에서는 Kotlin으로 두 날짜를 어떻게 비교하는지 확인할 수 있습니다.

```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2020, 1, 1)
    val date2 = LocalDate.of(2021, 1, 1)

    when {
        date1.isBefore(date2) -> println("date1이 date2보다 이전입니다.")
        date1.isAfter(date2) -> println("date1이 date2보다 이후입니다.")
        else -> println("date1과 date2는 동일한 날짜입니다.")
    }
}
```

실행 결과:

```Shell
date1이 date2보다 이전입니다.
```

## 깊게 알아보기

Kotlin에서 날짜를 비교하는 방법은 Java에서 시작된 java.time 패키지의 일부입니다. 이 패키지는 원래 Java 8에서 도입되었으며, Kotlin에서도 지원됩니다.

다양한 비교 방법이 있습니다. `isBefore()`, `isAfter()`, `isEqual()` 메서드 뿐만 아니라, `compareTo()` 메서드를 사용할 수도 있습니다. `compareTo()` 메서드는 두 날짜가 동일한 경우 0을 반환하고, 첫 번째 날짜가 두 번째 날짜보다 이전인 경우 음수를 반환하고, 그 반대의 경우 양수를 반환합니다.

또한 Kotlin은 연산자 오버로딩을 지원하기 때문에 날짜를 비교하는 데 표준 비교 연산자(<, >, == 등)를 사용할 수도 있습니다.

## 참고자료:

1. [Kotlin Documentation: java.time.LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/java.time/-local-date/)
2. [Oracle Documentation: java.time.LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
3. [Baeldung: Compare Dates in Java](https://www.baeldung.com/java-date-comparison)