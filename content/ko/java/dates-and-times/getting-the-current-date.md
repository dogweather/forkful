---
title:                "현재 날짜 가져오기"
aliases: - /ko/java/getting-the-current-date.md
date:                  2024-02-03T19:09:48.853364-07:00
model:                 gpt-4-0125-preview
simple_title:         "현재 날짜 가져오기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
