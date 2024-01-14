---
title:                "Java: 미래나 과거 날짜 계산하기"
simple_title:         "미래나 과거 날짜 계산하기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 이동하는 프로그래밍을 왜 해야 할까요? 오직1-2문장 설명합니다.

## 어떻게 하나요?

"```Java
import java.time.LocalDate;

public class DateCalculator {

     public static void main(String[] args) {
     
        // 오늘 날짜 가져오기
        LocalDate today = LocalDate.now();
        
        // 2주 전 날짜 계산
        LocalDate twoWeeksAgo = today.minusWeeks(2);
        System.out.println("Today: " + today);
        System.out.println("Two Weeks Ago: " + twoWeeksAgo);
        
        // 3일 후 날짜 계산
        LocalDate threeDaysLater = today.plusDays(3);
        System.out.println("Three Days Later: " + threeDaysLater);
        
        // 1년 전 날짜 계산
        LocalDate oneYearAgo = today.minusYears(1);
        System.out.println("One Year Ago: " + oneYearAgo);
     }
}
```"

출력:
Today: 2020-11-20
Two Weeks Ago: 2020-11-06
Three Days Later: 2020-11-23
One Year Ago: 2019-11-20

## 깊이있게 알아보기

미래 또는 과거의 날짜를 계산하는 것은 프로그래밍에서 매우 일반적인 작업입니다. Java에서는 java.time 패키지를 사용하여 LocalDate 클래스를 이용하여 쉽게 계산할 수 있습니다. 또한, 날짜를 다른 시간 단위로 계산하는 것도 가능합니다. 예를 들어, plusWeeks() 메소드를 이용하여 현재 날짜로부터 2주 후 날짜를 계산할 수 있고, minusDays() 메소드를 이용하여 현재 날짜로부터 3일 전 날짜를 계산할 수 있습니다. 또한, 날짜 뿐만 아니라 시간도 같이 계산할 수 있는 LocalDateTime 클래스를 이용하여 더욱 다양한 기능을 구현할 수 있습니다.

## 더 알아보기

### Java 날짜 및 시간 클래스: java.time 패키지
- https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
### Java 8 날짜 및 시간 API 소개
- https://www.baeldung.com/java-8-date-time-intro
### Java에서 현재 날짜와 시간 가져오기
- https://www.techiedelight.com/current-date-time-java/
### Calendars in Java: Creating a Date Calculator
- https://stackabuse.com/calendars-in-java-creating-a-date-calculator/