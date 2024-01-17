---
title:                "두 날짜를 비교하는 방법"
html_title:           "Java: 두 날짜를 비교하는 방법"
simple_title:         "두 날짜를 비교하는 방법"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

두 날짜를 비교하는 것은 날짜 간 차이를 계산하는 것을 의미합니다. 프로그래머들은 이를 통해 두 날짜의 관계를 파악하고, 필요한 조건에 따라 프로그램을 실행할 수 있습니다.

# 방법:

```Java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

LocalDate date1 = LocalDate.of(2020, 5, 15);
LocalDate date2 = LocalDate.of(2019, 3, 25);

// 두 날짜 간의 일 수 차이 계산
long daysBetween = ChronoUnit.DAYS.between(date2, date1);
System.out.println(daysBetween); // 출력: 417

// 두 날짜 간의 월 수 차이 계산
long monthsBetween = ChronoUnit.MONTHS.between(date2, date1);
System.out.println(monthsBetween); // 출력: 14

// 두 날짜 간의 연 수 차이 계산
long yearsBetween = ChronoUnit.YEARS.between(date2, date1);
System.out.println(yearsBetween); // 출력: 1
```

# 깊게 파헤치기:

(1) 역사적 맥락: 날짜 비교는 과거에는 어려운 작업이었습니다. 그래서 자바 8부터 등장한 java.time 패키지에서는 간단하고 편리한 날짜 계산을 제공합니다.

(2) 대안: 자바 8 이전에는 java.util 패키지의 Date 클래스를 사용하여 날짜 비교를 했습니다. 하지만 이 클래스는 버그가 많고 생각보다 사용하기 어렵습니다.

(3) 구현 세부사항: ChronoUnit 클래스는 날짜 비교를 위해 새로 나온 클래스입니다. 새로운 날짜API인 java.time 패키지 오브젝트들과 연동하여 사용하면 편리합니다.

# 더 알아보기:

- [Java 8 API 문서](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Java 포럼 - 자바 8 날짜/시간 API 소개](https://www.java.net/forum/topic/jdk/general/introducing-java-time-jdk-8)
- [코딩야학 - Java 8의 새로운 날짜 API 소개](https://www.codingyahak.com/java-new-date-time-api-introduction/)