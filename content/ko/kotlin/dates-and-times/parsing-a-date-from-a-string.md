---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:43.031475-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD55C\
  \uB2E4\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8\uB97C Date \uAC1D\uCCB4\uB85C \uBCC0\
  \uD658\uD558\uB294 \uC791\uC5C5\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uC774 \uC791\
  \uC5C5\uC740 \uC0AC\uC6A9\uC790\uAC00 \uC785\uB825\uD55C \uB0A0\uC9DC\uB098 \uC678\
  \uBD80 \uB370\uC774\uD130\uC14B\uC5D0\uC11C \uAC00\uC838\uC628 \uB0A0\uC9DC\uB97C\
  \ \uC27D\uAC8C \uC870\uC791\uD558\uACE0 \uD544\uC694\uC5D0 \uB530\uB77C \uD3EC\uB9F7\
  \uD305\uD560 \uC218 \uC788\uB3C4\uB85D \uD574\uC8FC\uBBC0\uB85C, \uB0A0\uC9DC\uC640\
  \ \uC0C1\uD638\uC791\uC6A9\uD558\uB294 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\
  \ \uC788\uC5B4 \uAE30\uBCF8\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4."
lastmod: 2024-02-19 22:05:14.097790
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD55C\uB2E4\
  \uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8\uB97C Date \uAC1D\uCCB4\uB85C \uBCC0\uD658\
  \uD558\uB294 \uC791\uC5C5\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uC774 \uC791\uC5C5\
  \uC740 \uC0AC\uC6A9\uC790\uAC00 \uC785\uB825\uD55C \uB0A0\uC9DC\uB098 \uC678\uBD80\
  \ \uB370\uC774\uD130\uC14B\uC5D0\uC11C \uAC00\uC838\uC628 \uB0A0\uC9DC\uB97C \uC27D\
  \uAC8C \uC870\uC791\uD558\uACE0 \uD544\uC694\uC5D0 \uB530\uB77C \uD3EC\uB9F7\uD305\
  \uD560 \uC218 \uC788\uB3C4\uB85D \uD574\uC8FC\uBBC0\uB85C, \uB0A0\uC9DC\uC640 \uC0C1\
  \uD638\uC791\uC6A9\uD558\uB294 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0 \uC788\
  \uC5B4 \uAE30\uBCF8\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열에서 날짜를 파싱한다는 것은 텍스트를 Date 객체로 변환하는 작업을 의미합니다. 이 작업은 사용자가 입력한 날짜나 외부 데이터셋에서 가져온 날짜를 쉽게 조작하고 필요에 따라 포맷팅할 수 있도록 해주므로, 날짜와 상호작용하는 애플리케이션에 있어 기본적인 작업입니다.

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
