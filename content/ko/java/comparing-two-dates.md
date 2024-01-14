---
title:                "Java: 두 날짜 비교하기"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

비교적으로 최근 날짜를 기준으로 과거의 날짜와 비교하는 경우, 두 날짜 사이의 차이를 알아야 할 때 자바 프로그래밍을 사용해서 가장 쉽고 빠른 방법을 제공해줍니다. 

## 어떻게

```java
// 두 날짜 생성
LocalDate pastDate = LocalDate.of(2020, 1, 1);
LocalDate currentDate = LocalDate.now();

// 두 날짜 사이의 차이 계산
long daysBetween = ChronoUnit.DAYS.between(pastDate, currentDate);

// 결과 출력
System.out.println("과거 날짜와 현재 날짜 사이의 차이는 " + daysBetween + "일 입니다.");
```

```java
// 날짜 포맷 설정
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

// 두 날짜 생성
LocalDate date1 = LocalDate.parse("2022-12-25", formatter);
LocalDate date2 = LocalDate.parse("2022-10-10", formatter);

// 두 날짜 사이의 차이 계산
long monthsBetween = ChronoUnit.MONTHS.between(date1, date2);

// 결과 출력
System.out.println("날짜 포맷을 지정하여 두 날짜 사이의 차이는 " + monthsBetween + "개월 입니다.");
```

## 자세히 살펴보기

자바의 날짜 및 시간 API인 `LocalDate`와 `ChronoUnit` 클래스를 사용하면 매우 간단하게 두 날짜를 비교할 수 있습니다. `between()` 메소드에는 두 날짜를 비교할 때 사용하고 싶은 시간 단위를 지정할 수 있습니다. 또한 `DateTimeFormatter` 클래스를 사용하면 원하는 날짜 형식으로 날짜를 입력받을 수 있습니다. 

## 관련 참고자료

- [Java 8 날짜 및 시간 API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Java 8의 새로운 날짜 및 시간 API를 사용하여 날짜 비교하기](https://www.baeldung.com/java-8-date-time-intro#comparing-dates-in-java8)
- [Java 8 날짜 및 시간 API를 활용한 간단한 예제](https://www.baeldung.com/java-8-date-time-intro#simple-example)