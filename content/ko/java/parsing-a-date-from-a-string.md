---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:36:47.133555-07:00
html_title:           "Arduino: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 날짜를 파싱하는 것은 텍스트 형식의 데이터를 구체적인 날짜 타입으로 변환하는 과정이다. 프로그래머가 이 작업을 수행하는 이유는 사용자로부터 혹은 파일로부터 받은 날짜 정보를 프로그램에서 사용 가능하게 처리하기 위해서다.

## How to (방법)
Java에서 문자열로부터 날짜를 파싱하려면 `java.time.format.DateTimeFormatter`와 `java.time.LocalDate` 또는 관련 클래스를 사용한다.

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

public class DateParsingExample {
    public static void main(String[] args) {
        String dateString = "2023-03-15";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        try {
            LocalDate date = LocalDate.parse(dateString, formatter);
            System.out.println("Parsed Date: " + date);
        } catch (DateTimeParseException e) {
            System.out.println("Error parsing the date: " + e.getMessage());
        }
    }
}
```

실행 결과:
```
Parsed Date: 2023-03-15
```

## Deep Dive (심화 학습)
자바에서 날짜 파싱 기능은 `java.util.Date`에서 시작되었다. 그러나 `java.util.Date`는 시간대 처리에 문제가 있고, 수정 불가능(Immutable)하지 않아서 `java.time` 패키지를 통해 새로운 날짜와 시간 API가 Java 8에서 도입되었다. `java.time.LocalDate`, `java.time.LocalDateTime`, `java.time.ZonedDateTime` 등 다양한 클래스가 있으며 상황에 따라 선택하여 사용할 수 있다. 날짜 형식 지정자를 사용하여 원하는 날짜 패턴으로 파싱할 수 있다. `DateTimeParseException`은 잘못된 형식의 문자열을 파싱할 때 발생하는 예외를 처리한다.

다른 방법으로는 `SimpleDateFormat` 클래스를 사용할 수 있지만, 이는 Java 8 이전의 코드에서 주로 볼 수 있는 방식이고, thread-safe하지 않아 권장하지 않는다.

구현 세부 사항으로는, `DateTimeFormatter` 클래스가 내부적으로 사용하는 패턴 문자열을 살펴보는 것이 중요하다. 예를 들어, `yyyy-MM-dd` 패턴은 ISO-8601 날짜 형식을 나타낸다.

## See Also (참고 자료)
- [java.time 패키지 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [`DateTimeFormatter` 클래스 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Java 날짜 및 시간 API 가이드](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
