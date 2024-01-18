---
title:                "문자열에서 날짜 파싱하기"
html_title:           "Kotlin: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜를 문자열로부터 추출한다는 것이 무엇인지 설명하려고 합니다. 프로그래머들이 이 작업을 왜 하는지도 알려 드리겠습니다.

날짜를 문자열로부터 추출한다는 것은, 문자열 안에 있는 날짜 정보를 프로그래밍 언어가 이해할 수 있는 형식으로 변환하는 것을 의미합니다. 프로그래머들이 이 작업을 하는 이유는 다양하지만, 대표적으로는 사용자 입력을 받아서 날짜 정보를 처리해야 할 때, 그리고 외부 API로부터 날짜 데이터를 받아와서 사용할 때가 있습니다.

## 하는 방법:
날짜를 문자열로부터 추출하는 방법을 간단한 코딩 예제와 함께 알아보겠습니다.

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val formatter = DateTimeFormatter.ofPattern("MM/dd/yyyy")
    val date = LocalDate.parse("09/30/2020", formatter)
    println(date)
}
```

위 코드는 ```"09/30/2020"```이라는 문자열에서 날짜 정보를 추출해오는 간단한 예제입니다. 먼저, ```DateTimeFormatter```를 사용하여 날짜 형식을 정의하고, 이를 이용해 ```LocalDate```를 생성합니다. 그리고 생성된 날짜 정보를 출력하는 것입니다.

출력결과: 
```
2020-09-30
```

## 심층 분석:
날짜를 문자열로부터 추출하는 작업은 오랜 역사를 가지고 있습니다. 과거에는 날짜 형식에 대한 표준이 없었기 때문에, 많은 어려움이 있었지만, 현재는 표준화된 방식으로 날짜 형식을 처리할 수 있게 되었습니다. Java의 ```SimpleDateFormat``` 클래스가 대표적인 예입니다. 하지만 이 클래스는 스레드 안전하지 않아서 멀티스레딩 환경에서 문제가 발생할 수 있습니다.

Kotlin에서는 Java의 ```Date```가 아닌 새로운 날짜 관련 클래스들을 제공하고 있으며, 쉽게 사용할 수 있도록 많은 기능들을 제공합니다. 예를 들어, 날짜를 문자열로 변환하기 위해서는 ```format()``` 함수를 사용하거나, 반대로 날짜를 파싱하기 위해서는 ```parse()``` 함수를 사용하면 됩니다.

더 많은 정보를 원한다면, Kotlin 공식 문서를 확인해 보세요.

## 관련 자료:
- [Kotlin 공식 문서](https://kotlinlang.org/docs/reference/basic-types.html#dates-and-times)
- [Java의 날짜 관련 클래스들](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java의 SimpleDateFormat 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)