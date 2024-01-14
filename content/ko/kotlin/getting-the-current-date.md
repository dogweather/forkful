---
title:                "Kotlin: 현재 날짜 받아오기"
simple_title:         "현재 날짜 받아오기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜: 현재 날짜를 구하는 것에 참여할 이유

현재 날짜를 구하는 것은 많은 프로그래밍 과제에서 필수적인 요소입니다. 예를 들어, 프로그램이 지정된 일정 날짜에 실행되도록하는 기능이 필요할 수 있습니다. 또는 현재 날짜를 표시하는 기능이 필요한 경우도 있습니다. 따라서 Kotlin에서 현재 날짜를 가져오는 방법을 배우는 것은 중요합니다.

## Kotlin에서 현재 날짜 가져오기

```Kotlin
import java.time.LocalDate

// 현재 날짜 가져오기
val date = LocalDate.now()

// YYYY-MM-DD 형식으로 날짜 출력
println("오늘의 날짜: $date")

// 날짜를 요일과 함께 출력하기
println("오늘의 날짜와 요일: ${date.dayOfWeek}, $date")
```

위의 코드는 Kotlin에서 현재 날짜를 가져오는 방법을 보여줍니다. `LocalDate` 클래스는 Kotlin 1.8 이상에서 사용할 수 있는 Java 8의 새로운 날짜 및 시간 API입니다. `now()` 메소드를 사용하여 현재 날짜를 가져올 수 있습니다. 그리고 `println()` 함수를 사용하여 날짜를 출력할 수 있습니다.

출력 예시:

```
오늘의 날짜: 2021-05-26
오늘의 날짜와 요일: WEDNESDAY, 2021-05-26
```

## 깊이 파고들기

Kotlin에서 현재 날짜를 가져오는 방법은 Java 8의 새로운 날짜 및 시간 API를 사용하여 단순하게 처리할 수 있습니다. 하지만 더 깊이 파고들어보면 `LocalDate` 클래스에는 날짜를 다루는 다양한 메소드들이 있습니다. 예를 들어, `plusDays()` 메소드를 사용하여 일 수를 더하거나 빼는 등의 연산이 가능합니다. 또한 `isBefore()` 나 `isAfter()`와 같은 메소드를 사용하여 날짜를 비교할 수도 있습니다.

## 관련 정보

마지막으로, Kotlin에서 현재 날짜를 가져오는 방법에 대해 학습하기 위해 더 많은 정보를 원하시는 분들을 위해 관련 링크를 제공합니다.

* [Kotlin Java 8에서 날짜 및 시간 다루기](https://kotlinlang.org/docs/java-interop.html#java-8-date-time-handling)
* [Java 8 날짜 및 시간 API 문서](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
* [Kotlin LocalDate 클래스 문서](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/index.html)

## 참고

이 글은 Kotlin 패스트캠퍼스에서 진행한 [Kotlin 101](https://fastcampus.co.kr/dev_online_kot/) 강의를 참고하여 작성되었습니다.