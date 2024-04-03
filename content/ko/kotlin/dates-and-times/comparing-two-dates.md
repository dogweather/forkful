---
date: 2024-01-20 17:33:29.179456-07:00
description: "How to? (\uC5B4\uB5BB\uAC8C?) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.189946-06:00'
model: gpt-4-1106-preview
summary: .
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
weight: 27
---

## How to? (어떻게?)
```kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2023, 4, 1)
    val date2 = LocalDate.now()

    println("Date 1: $date1")
    println("Date 2: $date2")

    when {
        date1.isBefore(date2) -> println("Date1이 Date2보다 이릅니다.")
        date1.isAfter(date2) -> println("Date1이 Date2보다 늦습니다.")
        else -> println("Date1과 Date2가 같습니다.")
    }
}
```
출력 예시:
```
Date 1: 2023-04-01
Date 2: 2023-04-12
Date1이 Date2보다 이릅니다.
```

## Deep Dive (심층 분석)
날짜 비교는 `java.util.Date`로 시작했지만, 부족함이 많았습니다. 그래서 `java.time.LocalDate`와 같은 새로운 API가 Java 8에서 등장했습니다. Kotlin도 Java의 이 API를 바탕으로 날짜를 더 편리하게 다룹니다. 두 날짜의 차이를 구하려면 `Period`나 `Duration` 클래스를 사용할 수 있습니다. 또한, 라이브러리 예를 들어 Joda-Time도 많이 쓰였는데, 이젠 자바 표준 API가 대부분의 기능을 제공합니다.

## See Also (더 보기)
- [Oracle JavaDocs: LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
