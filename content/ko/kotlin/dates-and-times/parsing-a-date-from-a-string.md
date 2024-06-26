---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:43.031475-07:00
description: "\uBC29\uBC95: Kotlin\uC740 Java 8\uC5D0\uC11C \uB3C4\uC785\uB41C `java.time`\
  \ \uD328\uD0A4\uC9C0\uB97C \uD1B5\uD574 \uB0A0\uC9DC \uD30C\uC2F1\uC744 \uC9C0\uC6D0\
  \uD569\uB2C8\uB2E4. \uC5EC\uAE30 `LocalDateTime`\uACFC \uD2B9\uC815 \uD328\uD134\
  \uC744 \uC0AC\uC6A9\uD55C \uAC04\uB2E8\uD55C \uC811\uADFC \uBC29\uBC95\uC774 \uC788\
  \uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.185887-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\uC740 Java 8\uC5D0\uC11C \uB3C4\uC785\uB41C `java.time` \uD328\uD0A4\
  \uC9C0\uB97C \uD1B5\uD574 \uB0A0\uC9DC \uD30C\uC2F1\uC744 \uC9C0\uC6D0\uD569\uB2C8\
  \uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 방법:
Kotlin은 Java 8에서 도입된 `java.time` 패키지를 통해 날짜 파싱을 지원합니다. 여기 `LocalDateTime`과 특정 패턴을 사용한 간단한 접근 방법이 있습니다:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateFromString(dateString)
    println(date)  // 출력: 2023-04-01T12:00
}
```

더 많은 유연성을 원하거나 API와 같은 외부 소스로부터 날짜를 처리하고자 한다면, `java.time`이 견고해졌음에도 Joda-Time과 같은 타사 라이브러리를 사용할 수 있습니다. 그러나 대부분의 Kotlin 애플리케이션에서는 JDK가 제공하는 현대적 방법을 고수하는 것이 선호됩니다.

Java 8 이전 버전용이거나 `java.time`을 지원하지 않는 Android API 레벨에서 타사 라이브러리를 사용하지 않고 Kotlin에서 날짜를 파싱하려면 `SimpleDateFormat` 클래스를 사용할 수도 있습니다:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // 출력 형식은 사용자의 시간대에 따라 다름, 예: Sat Apr 01 12:00:00 GMT 2023
}
```

`SimpleDateFormat`을 사용할 때에는 파싱된 날짜에서 예상치 못한 오프셋을 피하기 위해 항상 타임존을 설정하는 것을 잊지 마세요.
