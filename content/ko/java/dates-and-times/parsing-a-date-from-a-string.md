---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:24.054068-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD55C\
  \uB2E4\uB294 \uAC83\uC740 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC758 \uD14D\uC2A4\uD2B8\
  \ \uD45C\uD604\uC744 `Date` \uAC1D\uCCB4\uB098 \uB354 \uD604\uB300\uC801\uC778 `LocalDateTime`\
  \ \uAC1D\uCCB4\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uB0A0\
  \uC9DC\uB97C \uC870\uC791, \uD3EC\uB9F7, \uBE44\uAD50\uD558\uAC70\uB098 \uD45C\uC900\
  \uD654\uB41C \uD615\uC2DD\uC73C\uB85C \uC800\uC7A5\uD558\uAE30 \uC704\uD574 \uC774\
  \ \uC791\uC5C5\uC744 \uC218\uD589\uD558\uB294\uB370, \uC774\uB294\u2026"
lastmod: '2024-03-13T22:44:55.062950-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD55C\uB2E4\
  \uB294 \uAC83\uC740 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC758 \uD14D\uC2A4\uD2B8 \uD45C\
  \uD604\uC744 `Date` \uAC1D\uCCB4\uB098 \uB354 \uD604\uB300\uC801\uC778 `LocalDateTime`\
  \ \uAC1D\uCCB4\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uD1B5\uD574 \uB0A0\
  \uC9DC\uB97C \uC870\uC791, \uD3EC\uB9F7, \uBE44\uAD50\uD558\uAC70\uB098 \uD45C\uC900\
  \uD654\uB41C \uD615\uC2DD\uC73C\uB85C \uC800\uC7A5\uD558\uAE30 \uC704\uD574 \uC774\
  \ \uC791\uC5C5\uC744 \uC218\uD589\uD558\uB294\uB370, \uC774\uB294\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

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
