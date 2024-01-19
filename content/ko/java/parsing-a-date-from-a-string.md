---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
문자열에서 날짜를 파싱하는 것은 문자 형식의 날짜를 날짜/시간 데이터 타입으로 변환하는 과정입니다. 이를 통해 프로그래머들은 적절한 포맷으로 데이터를 조작하고 계산할 수 있습니다.

## 어떻게 하는 가:
Java에서는 `java.time` 패키지의 `LocalDate` 클래스를 사용하여 문자열에서 날짜를 파싱할 수 있습니다. 간단한 예를 보겠습니다:

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Main {
    public static void main(String[] args) {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        String dateInString = "2018-06-25";
        LocalDate date = LocalDate.parse(dateInString, formatter);
        System.out.println(date);
    }
}
```
출력 결과는 다음과 같습니다:
```shell
2018-06-25
```

## 깊이 파고들기:
날짜를 파싱하는 개념은 프로그래밍의 초기 시절부터 있었습니다. 단순한 문자열을 실제 날짜로 변환함으로써, 프로그래머는 날짜에 관련된 다양한 작업을 쉽게 수행할 수 있습니다. `java.time` 패키지의 도입으로 Java에서의 날짜 파싱이 매우 간결해졌습니다.
이외에도, `java.text.SimpleDateFormat` 클래스를 사용하여 날짜를 파싱하는 또 다른 방법이 있습니다. 그러나 현대 Java에서는 `java.time` 패키지를 권장합니다. 이는 보다 강력하고, 유연하며, 편리한 API 제공하기 때문입니다.
디테일을 더 들어가자면, `LocalDate.parse()` 메소드는 문자열과 패턴을 받아서 그에 따라 날짜를 파싱합니다. 이는 날짜뿐만 아니라 시간을 파싱하는 것도 가능합니다.

## 참고 자료:
1. Official Oracle Documentation on LocalDate: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
2. Baeldung Guide on Parsing Date and Time: https://www.baeldung.com/java-date
3. JournalDev Explanation on Java DateTimeFormatter: https://www.journaldev.com/17899/java-datetimeformatter