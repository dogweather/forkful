---
date: 2024-01-20 17:33:26.345705-07:00
description: "\uB450 \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD55C\uB2E4\uB294 \uAC83\uC740\
  \ \uCE98\uB9B0\uB354 \uC0C1\uC5D0\uC11C \uB450 \uB0A0\uC9DC\uAC00 \uC5B4\uB5BB\uAC8C\
  \ \uC11C\uB85C \uAD00\uACC4\uB418\uB294\uC9C0\uB97C \uD30C\uC545\uD558\uB294 \uAC83\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uAE30\uD55C \uAC80\
  \uC0AC, \uC2DC\uAC04 \uCC28\uC774 \uACC4\uC0B0, \uC815\uB82C \uBC0F \uB370\uC774\
  \uD130 \uC720\uD6A8\uC131 \uAC80\uC99D \uB4F1\uC744 \uD560 \uB54C \uB450 \uB0A0\uC9DC\
  \uB97C \uBE44\uAD50\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:13.977163
model: gpt-4-1106-preview
summary: "\uB450 \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD55C\uB2E4\uB294 \uAC83\uC740 \uCE98\
  \uB9B0\uB354 \uC0C1\uC5D0\uC11C \uB450 \uB0A0\uC9DC\uAC00 \uC5B4\uB5BB\uAC8C \uC11C\
  \uB85C \uAD00\uACC4\uB418\uB294\uC9C0\uB97C \uD30C\uC545\uD558\uB294 \uAC83\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uAE30\uD55C \uAC80\uC0AC\
  , \uC2DC\uAC04 \uCC28\uC774 \uACC4\uC0B0, \uC815\uB82C \uBC0F \uB370\uC774\uD130\
  \ \uC720\uD6A8\uC131 \uAC80\uC99D \uB4F1\uC744 \uD560 \uB54C \uB450 \uB0A0\uC9DC\
  \uB97C \uBE44\uAD50\uD569\uB2C8\uB2E4."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why?
두 날짜를 비교한다는 것은 캘린더 상에서 두 날짜가 어떻게 서로 관계되는지를 파악하는 것입니다. 프로그래머들은 기한 검사, 시간 차이 계산, 정렬 및 데이터 유효성 검증 등을 할 때 두 날짜를 비교합니다.

## How to:
```Java
import java.time.LocalDate;
import java.time.Period;

public class DateComparison {
    public static void main(String[] args) {
        LocalDate date1 = LocalDate.of(2023, 3, 15);
        LocalDate date2 = LocalDate.now();

        // Checking if two dates are equal
        boolean isEqual = date1.isEqual(date2);
        System.out.println("Dates equal: " + isEqual);

        // Checking if one date is before another
        boolean isBefore = date1.isBefore(date2);
        System.out.println("Date1 is before Date2: " + isBefore);

        // Checking if one date is after another
        boolean isAfter = date1.isAfter(date2);
        System.out.println("Date1 is after Date2: " + isAfter);

        // Calculating the period between two dates
        Period period = Period.between(date1, date2);
        System.out.println("Period: " + period.getYears() + " Years " 
            + period.getMonths() + " Months " + period.getDays() + " Days");
    }
}
```
출력 예시:
```
Dates equal: false
Date1 is before Date2: true
Date1 is after Date2: false
Period: 0 Years 0 Months 2 Days
```

## Deep Dive
날짜 비교는 Java 8 이전에는 `java.util.Date`와 `java.util.Calendar`를 사용하여 수행했습니다. Java 8에서 `java.time` 패키지는 불변(immutable)인 `LocalDate`, `LocalDateTime`, `Instant` 클래스를 소개했고, 이는 더 강력하고 명확하며 사용하기 쉽습니다.

`java.time.LocalDate`는 시간대 정보를 포함하지 않고 오직 날짜만을 나타냅니다. 그래서 단순한 날짜 비교에는 최적화된 클래스입니다. `Period` 클래스는 두 날짜 사이의 기간을 년, 월, 일 단위로 표현할 수 있게 합니다. 

`isEqual`, `isBefore`, `isAfter` 메소드는 LocalDate의 인스턴스에 대한 간단하지만 강력한 비교 연산을 제공합니다.

하지만 `java.time` 패키지는 모든 상황에 대해 완벽한 것은 아닙니다. 시간대를 고려해야할 때는 `ZonedDateTime`이 필요하며, 보다 복잡한 날짜 연산은 `java.time.temporal` 패키지를 사용하여 수행할 수 있습니다.

## See Also
- [LocalDate 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Period 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/time/Period.html)
- [Java 8 Date-Time API 소개](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
