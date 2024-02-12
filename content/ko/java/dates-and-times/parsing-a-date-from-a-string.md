---
title:                "문자열에서 날짜 분석하기"
aliases:
- /ko/java/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:24.054068-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열에서 날짜 분석하기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열에서 날짜를 파싱한다는 것은 날짜와 시간의 텍스트 표현을 `Date` 객체나 더 현대적인 `LocalDateTime` 객체로 변환하는 것을 의미합니다. 프로그래머들은 이를 통해 날짜를 조작, 포맷, 비교하거나 표준화된 형식으로 저장하기 위해 이 작업을 수행하는데, 이는 날짜 계산, 유효성 검사 또는 일관된 국제화를 요구하는 애플리케이션에 필수적입니다.

## 방법:

### `java.time` 패키지 사용하기 (Java 8 이후 권장):
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // 출력: 2023-04-30
    }
}
```

### `SimpleDateFormat` 사용하기 (오래된 방법):
```java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "30/04/2023";
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        try {
            Date date = formatter.parse(dateString);
            System.out.println(date); // 출력 형식은 시스템의 기본 형식에 따라 다름
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### 타사 라이브러리 사용하기 (예: Joda-Time):
Joda-Time은 중요한 타사 라이브러리였지만, Java 8에서 `java.time` 패키지가 도입되면서 현재 유지 보수 모드에 있습니다. 하지만, Java 8 이전 버전을 사용하는 경우 Joda-Time은 좋은 선택입니다.
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // 출력: 2023-04-30
    }
}
```
날짜와 작업할 때, 단순한 날짜뿐만 아니라 날짜-시간을 파싱하거나 포맷팅할 경우 항상 시간대 설정을 인지하고 있어야 합니다.
