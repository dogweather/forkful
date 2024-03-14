---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:48.853364-07:00
description: "Java\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uC5BB\uB294 \uAC83\
  \uC740 \uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uB85C\uAE45, \uB0A0\uC9DC \uACC4\uC0B0\
  , \uC2DC\uAC04 \uAE30\uBC18 \uC870\uAC74 \uB4F1\uC758 \uC791\uC5C5\uC5D0 \uB0A0\uC9DC\
  \ \uAC1D\uCCB4\uB97C \uC870\uC791\uD560 \uC218 \uC788\uAC8C \uD574\uC8FC\uB294 \uAE30\
  \uBCF8\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uCD94\uC801, \uC2A4\uCF00\uC904\
  \uB9C1 \uBC0F \uC2DC\uAC04 \uB370\uC774\uD130 \uBD84\uC11D\uC774 \uC911\uC694\uD55C\
  \ \uC751\uC6A9 \uD504\uB85C\uADF8\uB7A8\uC5D0\uC11C \uD544\uC218\uC801\uC785\uB2C8\
  \uB2E4."
lastmod: '2024-03-13T22:44:55.064568-06:00'
model: gpt-4-0125-preview
summary: "Java\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uC5BB\uB294 \uAC83\uC740\
  \ \uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uB85C\uAE45, \uB0A0\uC9DC \uACC4\uC0B0,\
  \ \uC2DC\uAC04 \uAE30\uBC18 \uC870\uAC74 \uB4F1\uC758 \uC791\uC5C5\uC5D0 \uB0A0\uC9DC\
  \ \uAC1D\uCCB4\uB97C \uC870\uC791\uD560 \uC218 \uC788\uAC8C \uD574\uC8FC\uB294 \uAE30\
  \uBCF8\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uCD94\uC801, \uC2A4\uCF00\uC904\
  \uB9C1 \uBC0F \uC2DC\uAC04 \uB370\uC774\uD130 \uBD84\uC11D\uC774 \uC911\uC694\uD55C\
  \ \uC751\uC6A9 \uD504\uB85C\uADF8\uB7A8\uC5D0\uC11C \uD544\uC218\uC801\uC785\uB2C8\
  \uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
Java에서 현재 날짜를 얻는 것은 프로그래머가 로깅, 날짜 계산, 시간 기반 조건 등의 작업에 날짜 객체를 조작할 수 있게 해주는 기본적인 작업입니다. 추적, 스케줄링 및 시간 데이터 분석이 중요한 응용 프로그램에서 필수적입니다.

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
