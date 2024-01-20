---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 뭐하고 왜?

날짜를 문자열로 변환하는 것은 날짜 또는 시간 정보를 사람이 읽기 쉬운 형식의 문자열로 변경하는 과정입니다. 프로그래머들이 이를 수행하는 주된 이유는 사용자 친화적인 형식으로 날짜 정보를 표시하고, 로그를 추적하거나 사건을 쉽게 시간별로 정렬하기 위함입니다.

## 이렇게 하세요:

```Kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val current = LocalDateTime.now()
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val formatted = current.format(formatter)

    println("현재 날짜 및 시간: " + current)
    println("포매팅 된 날짜 및 시간: " + formatted)
}
```
예시 실행 결과는 다음과 같습니다:

```
현재 날짜 및 시간: 2022-06-25T15:23:01.123
포매팅 된 날짜 및 시간: 2022-06-25 15:23:01
```

## 깊게 들어가보기

이제 우리는 날짜를 문자열로 변환하는 기본 개념을 이해했습니다. 하지만 이 주제는 그 이상의 이해를 요구합니다. 

### 역사적 맥락

주요 시간 관련 라이브러리가 제공하는 클래스와 기능은 시간의 표현에 관한 역사적 경험을 바탕으로 개발되었습니다.

### 대안

각 프로그램 언어는 대다수가 날짜와 시간 변환 방식을 제공합니다. Python에는 strftime라는 유용한 메소드가 있고, JavaScript에는 toLocaleString이 있습니다.

### 구현 세부사항

날짜를 문자열로 변환하는 것은 간단할 수 있지만, 주의해야 할 점들이 있습니다. 원하는 출력 결과가 어떤 것인지, 특히 날짜와 시간의 표시 방식이 국가 및 문화에 따라 다르기 때문에 항상 명확해야 합니다.

## 참고자료

유용한 추가 정보를 찾을 수 있는 몇 가지 링크를 제공합니다:

1. Kotlin 공식 날짜 및 시간 처리 가이드: <https://kotlinlang.org/docs/dates-times.html>  

2. Java 8 에서 도입된 새로운 날짜 및 시간 API 대해 이해하기: <https://www.baeldung.com/java-8-date-time-intro>

3. 공식 Java 문서에서 DateTimeFormatter 클래스 배우기: <https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/format/DateTimeFormatter.html>