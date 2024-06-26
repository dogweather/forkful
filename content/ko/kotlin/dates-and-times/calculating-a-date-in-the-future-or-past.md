---
date: 2024-01-20 17:31:43.611931-07:00
description: "How to (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Kotlin\uC744 \uC0AC\uC6A9\
  \uD574 \uB0A0\uC9DC\uB97C \uACC4\uC0B0\uD558\uB294 \uC608\uC81C\uC785\uB2C8\uB2E4\
  . Java\uC758 `LocalDateTime`\uACFC `Period` \uD074\uB798\uC2A4\uB97C \uD65C\uC6A9\
  \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T22:38:55.931975-06:00'
model: gpt-4-1106-preview
summary: "How to (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Kotlin\uC744 \uC0AC\uC6A9\
  \uD574 \uB0A0\uC9DC\uB97C \uACC4\uC0B0\uD558\uB294 \uC608\uC81C\uC785\uB2C8\uB2E4\
  . Java\uC758 `LocalDateTime`\uACFC `Period` \uD074\uB798\uC2A4\uB97C \uD65C\uC6A9\
  \uD569\uB2C8\uB2E4."
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
weight: 26
---

## How to (어떻게 하나요?)
Kotlin을 사용해 날짜를 계산하는 예제입니다. Java의 `LocalDateTime`과 `Period` 클래스를 활용합니다.

```Kotlin
import java.time.LocalDateTime
import java.time.Period

fun main() {
    val today = LocalDateTime.now()
    println("오늘: $today")

    val tenDaysLater = today.plusDays(10)
    println("10일 후: $tenDaysLater")

    val threeWeeksEarlier = today.minusWeeks(3)
    println("3주 전: $threeWeeksEarlier")

    val period = Period.of(1, 2, 15) // 1년 2개월 15일
    val customDate = today.plus(period)
    println("1년 2개월 15일 후: $customDate")
}
```

출력 결과:
```
오늘: 2023-04-11T16:40:42.123
10일 후: 2023-04-21T16:40:42.123
3주 전: 2023-03-21T16:40:42.123
1년 2개월 15일 후: 2024-06-26T16:40:42.123
```

## Deep Dive (심도 있는 정보)
캘린더 계산은 컴퓨터 과학과 인류 역사 깊숙이 관련이 있습니다. 예로, 율리우스력과 그레고리력의 전환은 날짜 계산 방식에 영향을 미쳤습니다. Kotlin에서 날짜를 계산할 때 Java의 `java.time` 패키지(`LocalDate`, `LocalDateTime`, `Period`, `Duration` 등)를 활용하는 것이 일반적입니다. `java.util.Date`와 `Calendar`는 오래되었고 문제가 많아서 피하는 것이 좋습니다. `LocalDateTime` 클래스는 시간대를 고려하지 않는 날짜와 시간을 나타냅니다. 전 세계 사용자를 위해서는 `ZonedDateTime`을 사용해야 할 경우도 있습니다.

## See Also (참고 자료)
- [Kotlin 공식 문서](https://kotlinlang.org/docs/home.html)
- [java.time 패키지 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Date and Time API 가이드](https://www.baeldung.com/java-8-date-time-intro)
