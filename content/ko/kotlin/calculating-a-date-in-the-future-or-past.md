---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Kotlin: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

날짜 계산은 미래나 과거의 특정 날짜를 찾는 활동입니다. 프로그래머들이 이를 수행하는 이유는 예약, 이벤트, 기념일 등에 대한 알림을 생성하거나 확인하는 데 필요하기 때문입니다.

## 사용 방법:

```Kotlin
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

fun main() {
    val now = LocalDateTime.now()
    val nextWeek = now.plus(1, ChronoUnit.WEEKS)
    val lastYear = now.minus(1, ChronoUnit.YEARS)

    println("현재 시간: $now")
    println("다음 주: $nextWeek")
    println("지난 해: $lastYear")
}
```

출력은 아래와 같이 보일 것입니다:

```
현재 시간: 2023-04-22T10:20:30.345
다음 주: 2023-04-29T10:20:30.345
지난 해: 2022-04-22T10:20:30.345
```

## 심화 학습:

과거와 미래의 날짜 계산은 컴퓨터 프로그래밍의 흥미로운 부분입니다. 과거에는 운영 체제와 시스템 설정에 의존하는 더 단순한 접근법을 사용했지만, JDK 8 이후로 Java는 새롭고 향상된 날짜 및 시간 API를 도입하였습니다. Kotlin 프로그래밍에서 이를 활용할 수 있습니다.

한편, 위 방법 외에도 Java Calendar 또는 Joda-Time 라이브러리 등 다양한 방법을 사용할 수 있습니다.

## 참고 자료:

1. Kotlin 공식 문서: [Kotlin Date and Time API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-duration/index.html)
2. Oracle 공식 문서: [Java Date Time API](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html)
3. 스택 오버플로우: [Kotlin에서 시간데이터 가장 활용하기 좋은 방법](https://stackoverflow.com/questions/58789502/what-is-the-best-way-to-deal-with-date-time-in-kotlin)
4. [Kotlin-Android 날짜 및 시간 API 활용 가이드](https://www.raywenderlich.com/6640148-working-with-date-and-time-in-kotlin-on-android)