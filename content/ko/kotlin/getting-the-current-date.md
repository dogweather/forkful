---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 해야 하는가?
현재 날짜를 가져오는 것은 현재 시스템의 날짜를 프로그램에서 확인할 수 있게 하는 작업입니다. 이렇게 하는 이유는 사용자에게 정확한 타임스탬프를 제공하거나 타임스탬프를 기반으로 한 작업을 수행하기 위함입니다.

## 어떻게 하는가:
Kotlin에서 현재 날짜를 가져오기 위한 코드 예시와 그 출력은 다음과 같습니다.

```Kotlin
import java.time.LocalDate

fun main() {
val currentDate = LocalDate.now()
println("현재 날짜: $currentDate")
}
```

출력은 다음과 같습니다.

``` 
현재 날짜: 2022-04-18
```
현지화된 날짜 형식을 얻으려면 `DateTimeFormatter`를 사용하세요.

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val currentDate = LocalDate.now()
    val formatter = DateTimeFormatter.ofPattern("yyyy.MM.dd")
    println("현재 날짜: ${currentDate.format(formatter)}")
}
```

출력은 다음과 같습니다.

```
현재 날짜: 2022.04.18
```

## 깊이 들어가보기:
자바에서 날짜와 시간을 다루기 위한 클래스들은 오랜 시간 동안 여러 가지 문제를 가지고 있었습니다. `java.util.Date`가 있었지만 불완전했고, 그 후에 등장한 `java.util.Calendar` 역시 최적의 솔루션이 아니었습니다. 그래서 Java 8에서 새로운 날짜와 시간 API가 도입되었고, 이는 JSR 310 스펙을 따릅니다. 이것이 `java.time.LocalDate` 입니다. 

대안으로는 Joda-Time 라이브러리가 있지만, 현재는 Java의 기본 날짜와 시간 API를 사용하는 것이 권장됩니다.

`LocalDate.now()`의 구현 세부 사항을 보면, 내부적으로는 시스템의 기본 시계를 사용하여 현재 날짜를 가져옵니다. 그리고 이 시계는 사용자 환경의 기본 시간대를 사용하여 현재 시간을 계산합니다.

## 참고자료:
- 자바 8 날짜와 시간 API 가이드: [https://www.baeldung.com/java-8-date-time-intro](https://www.baeldung.com/java-8-date-time-intro)
- Joda-Time 라이브러리: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)