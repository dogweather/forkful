---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:37:34.505957-07:00
html_title:           "Arduino: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 사용하죠?)
문자열에서 날짜를 파싱한다는 건, 텍스트 데이터 포맷의 날짜를 프로그램이 이해하고 사용할 수 있는 날짜 객체로 변환하는 것입니다. 데이터 교환, 사용자 입력 처리, 날짜 포맷 변경 등 다양한 이유로 프로그래머들이 이 작업을 수행합니다.

## How to: (어떻게 하나요?)
```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "2023-04-02"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val parsedDate = LocalDate.parse(dateString, formatter)
    
    println("날짜 객체: $parsedDate")
}
```
출력:
```
날짜 객체: 2023-04-02
```

## Deep Dive (더 알아보기)
문자열로부터 날짜를 파싱하는 기능은 Java 8에 도입된 `java.time` 패키지가 표준으로 사용되기 전에는 `java.util.Date`와 `java.text.SimpleDateFormat`이 주로 사용되었습니다. `java.time`은 보다 직관적이고 오류를 방지하는 설계 덕분에 현재 가장 널리 채택되고 있습니다. 날짜 파싱과 관련된 구현 세부사항으로는, 파싱할 때 주의해야 하는 날짜 형식, 시간대 문제, 유연한 파싱을 위한 `DateTimeFormatterBuilder` 등이 있습니다. 대안으로는 Joda-Time 라이브러리가 있었으나, 대부분은 이제 `java.time`으로 이동했습니다.

## See Also (추가 정보)
- [Kotlin 공식 문서](https://kotlinlang.org/docs/home.html)
- [`java.time` 패키지 문서](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Joda-Time 라이브러리](https://www.joda.org/joda-time/)
