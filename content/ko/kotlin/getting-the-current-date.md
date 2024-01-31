---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:15:53.711835-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"

category:             "Kotlin"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
현재 날짜 가져오기는 시스템의 현재 날짜를 얻는 것입니다. 로그 기록, 특정 기능의 타이밍, 사용자 인터페이스 활용 등 다양한 이유로 프로그래머들이 이 데이터를 사용합니다.

## How to (방법)
Kotlin에서 현재 날짜를 얻으려면 `java.time.LocalDate` 또는 `java.util.Calendar` 같은 자바 라이브러리를 사용합니다. 간결하게 코드를 작성해보죠.

```Kotlin
import java.time.LocalDate

fun main() {
    val currentDate = LocalDate.now()
    println("Today's Date: $currentDate")
}
```
위 코드의 출력 예시:
```
Today's Date: 2023-03-10
```

또 다른 방법:
```Kotlin
import java.util.Calendar

fun main() {
    val calendar = Calendar.getInstance()
    val currentDate = "${calendar.get(Calendar.YEAR)}-${calendar.get(Calendar.MONTH) + 1}-${calendar.get(Calendar.DAY_OF_MONTH)}"
    println("Today's Date: $currentDate")
}
```
이 코드에서는 `MONTH`에 +1을 해야하는 점 유의하세요. `Calendar.MONTH`는 0부터 시작합니다.

## Deep Dive (심층 분석)
Java 플랫폼에는 오래된 `java.util.Date`와 `java.util.Calendar`부터 최신의 `java.time` 패키지까지 다양한 날짜와 시간 관련 클래스가 있습니다. `java.time` 패키지는 불변 객체(immutable objects)와 thread-safe한 설계로 자바 8부터 도입되었습니다. `java.util.Calendar`는 오래되었고, 사용하기 복잡하지만 여전히 널리 사용되고 있습니다. 이 둘의 주요 차이점은 `java.time` 패키지가 더 직관적이고 오류를 줄일 수 있는 설계를 가지고 있다는 것입니다.

## See Also (더 보기)
- [Kotlin Docs](https://kotlinlang.org/docs/home.html) - 공식 Kotlin 문서에서 다양한 기능과 사용 예를 찾아볼 수 있습니다.
- [Oracle Java Date/Time API Guide](https://docs.oracle.com/javase/tutorial/datetime/) - 자바의 날짜와 시간 API에 대한 더 많은 정보가 있습니다.
- [Baeldung Kotlin Tutorials](https://www.baeldung.com/kotlin) - Kotlin 관련 다양한 튜토리얼 및 가이드가 있습니다.
