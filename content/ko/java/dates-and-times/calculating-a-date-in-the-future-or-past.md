---
date: 2024-01-20 17:31:13.867629-07:00
description: "How to: (\uBC29\uBC95) Java\uC5D0\uC11C\uB294 `LocalDate`, `LocalTime`,\
  \ `LocalDateTime`, `Period`, `Duration` \uD074\uB798\uC2A4\uB85C \uB0A0\uC9DC \uACC4\
  \uC0B0\uC744 \uC27D\uAC8C \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uCF54\uB4DC \uC608\
  \uC2DC\uB97C \uC0B4\uD3B4\uBCF4\uC138\uC694."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.068582-06:00'
model: gpt-4-1106-preview
summary: "Java\uC5D0\uC11C\uB294 `LocalDate`, `LocalTime`, `LocalDateTime`, `Period`,\
  \ `Duration` \uD074\uB798\uC2A4\uB85C \uB0A0\uC9DC \uACC4\uC0B0\uC744 \uC27D\uAC8C\
  \ \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
weight: 26
---

## How to: (방법)
Java에서는 `LocalDate`, `LocalTime`, `LocalDateTime`, `Period`, `Duration` 클래스로 날짜 계산을 쉽게 할 수 있습니다. 코드 예시를 살펴보세요.

```java
import java.time.LocalDate;
import java.time.Period;

public class DateCalculator {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        Period tenDays = Period.ofDays(10);
        
        LocalDate tenDaysLater = today.plus(tenDays);
        System.out.println("Ten days from now: " + tenDaysLater);

        LocalDate tenDaysBefore = today.minus(tenDays);
        System.out.println("Ten days ago: " + tenDaysBefore);
    }
}
```

출력:
```
Ten days from now: 2023-04-21
Ten days ago: 2023-03-31
```

## Deep Dive (심층 분석)
날짜 계산의 필요성은 컴퓨터 과학 초기부터 있었습니다. 그러나 초기에는 주로 `Date` 클래스를 사용했으나 이후 `Calendar` 클래스로 넘어갔고, 지금은 `java.time` 패키지가 더 새롭고 강력한 API를 제공합니다.

`java.time`은 자바 8 부터 도입되었으며, Joda-Time 라이브러리의 영향을 받았습니다. 이 패키지는 불변 객체(immutability)와 쓰레드 안전(thread safety)을 제공합니다.

`Period`와 `Duration`은 시간의 양을 표현합니다. `Period`는 연도, 월, 일을 기준으로 한 시간 차이를, `Duration`은 시, 분, 초를 기준으로 합니다.

알터너티브로, Joda-Time이나 Apache Commons Lang의 `DateUtils` 등 다른 라이브러리들을 사용할 수 있으나, 현대적인 자바 프로젝트는 주로 `java.time` 패키지를 추천합니다.

## See Also (더 보기)
- [Oracle's Java Tutorials for Date Time](https://docs.oracle.com/javase/tutorial/datetime/)
- [Joda-Time Documentation](https://www.joda.org/joda-time/)
- [Apache Commons Lang DateUtils](https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/time/DateUtils.html)
