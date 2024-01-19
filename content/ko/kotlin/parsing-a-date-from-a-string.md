---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열에서 날짜를 파싱하는 것은 특정 형식의 문자열을 날짜와 시간 객체로 변환하는 과정입니다. 프로그래머가 이 작업을 하면 문자열이 제시하는 정보를 프로그램으로 조작할 수 있습니다.

## 어떻게:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val string = "2022-03-25"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val date = LocalDate.parse(string, formatter)
    
    println(date)
}
```
이 코드 실행 결과는 다음과 같습니다:
```
2022-03-25
```

## 깊게 파헤치기:

날짜 파싱은 문자열에서 시간 요소를 추출하는 데 사용되는 작업입니다. 과거에는 `SimpleDateFormat`을 사용하여 문자열에서 날짜를 파싱했습니다. 하지만 이는 스레드에 안전하지 않고 사용하기 복잡했습니다. 그 대안으로 `java.time` 패키지가 Java 8에서 도입되었습니다. 위의 코틀린 코드도 `java.time` 클래스를 사용하여 문자열에서 날짜를 파싱하고 있습니다.

그 대체 기능으로 `DateUtils.parseDateStrictly`를 사용합니다. 이 함수는 `DateFormat` 객체를 사용하여 여러 패턴을 시도합니다. 그러나 `java.time` 패키지가 나오면서 이 보다 더 좋은 대안이 유효해졌습니다.

## 관련 자료: 

1. Java 8 DateTime API: https://docs.oracle.com/javase/tutorial/datetime/index.html
2. DateFormat 클래스 도큐먼트: https://developer.android.com/reference/java/text/DateFormat
3. 원문 Kotlin 공식 문서: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-date-time-format/index.html