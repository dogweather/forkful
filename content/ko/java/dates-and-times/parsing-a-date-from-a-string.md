---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:24.054068-07:00
description: "\uBC29\uBC95: #."
lastmod: '2024-03-13T22:44:55.062950-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

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
