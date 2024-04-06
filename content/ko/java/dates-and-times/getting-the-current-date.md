---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:48.853364-07:00
description: "\uBC29\uBC95: Java\uB294 \uAE30\uC874 `java.util.Date` \uD074\uB798\uC2A4\
  \uC640 \uB354 \uB2E4\uC591\uD558\uACE0 \uC9C1\uAD00\uC801\uC778 \uC0C8\uB85C\uC6B4\
  \ `java.time` \uD328\uD0A4\uC9C0(Java 8\uC5D0\uC11C \uB3C4\uC785)\uB97C \uC0AC\uC6A9\
  \uD558\uC5EC \uD604\uC7AC \uB0A0\uC9DC\uB97C \uC5BB\uB294 \uC5EC\uB7EC \uBC29\uBC95\
  \uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.064568-06:00'
model: gpt-4-0125-preview
summary: "Java\uB294 \uAE30\uC874 `java.util.Date` \uD074\uB798\uC2A4\uC640 \uB354\
  \ \uB2E4\uC591\uD558\uACE0 \uC9C1\uAD00\uC801\uC778 \uC0C8\uB85C\uC6B4 `java.time`\
  \ \uD328\uD0A4\uC9C0(Java 8\uC5D0\uC11C \uB3C4\uC785)\uB97C \uC0AC\uC6A9\uD558\uC5EC\
  \ \uD604\uC7AC \uB0A0\uC9DC\uB97C \uC5BB\uB294 \uC5EC\uB7EC \uBC29\uBC95\uC744 \uC81C\
  \uACF5\uD569\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

## 방법:
Java는 기존 `java.util.Date` 클래스와 더 다양하고 직관적인 새로운 `java.time` 패키지(Java 8에서 도입)를 사용하여 현재 날짜를 얻는 여러 방법을 제공합니다.

### `java.time.LocalDate` 사용하기
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // 예제 출력: 2023-04-01
    }
}
```

### `java.time.LocalDateTime` 사용하기
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // 예제 출력: 2023-04-01T12:34:56.789
    }
}
```

### `java.util.Date` 사용하기 (레거시)
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // 예제 출력: 토요일 4월 01 12:34:56 BST 2023
    }
}
```

### 서드파티 라이브러리 사용하기: Joda-Time
Java 8 이전에는 Joda-Time이 Java에서 날짜와 시간을 다루는 실질적인 표준이었습니다. 레거시 시스템에서 작업하거나 Joda-Time을 선호하는 경우 현재 날짜를 얻는 방법은 다음과 같습니다:
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // 예제 출력: 2023-04-01
    }
}
```
**참고:** `java.util.Date`와 Joda-Time이 여전히 사용되지만, 불변성과 날짜 및 시간 처리를 위한 포괄적인 API를 제공하기 때문에 새 프로젝트에는 `java.time` 패키지가 권장됩니다.
