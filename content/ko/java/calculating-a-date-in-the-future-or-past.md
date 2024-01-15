---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Java: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜
우리는 인간처럼 살기 때문에 현실에서는 미래와 과거의 날짜를 계산해야 할 때가 있습니다. 다음에는 자바 프로그래밍을 사용하여 간단한 미래와 과거 날짜 계산 방법을 배워보겠습니다.

## 어떻게
먼저, 미래 날짜를 계산하는 방법을 알아보겠습니다. ```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateCalculator {
    public static void main(String[] args) {
        // 미래 날짜를 계산하고 싶은 날짜와 일수를 지정합니다.
        LocalDate date = LocalDate.of(2021, 10, 10);
        int daysToAdd = 14;

        // 지정한 날짜에 일수를 더하고 포맷을 지정하여 출력합니다.
        LocalDate futureDate = date.plusDays(daysToAdd);
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy년 MM월 dd일");
        System.out.println("미래 날짜: " + futureDate.format(formatter));
    }
}
```

위 코드를 실행하면, 출력 결과는 다음과 같습니다.
```
미래 날짜: 2021년 10월 24일
```

이번에는 과거 날짜를 계산하는 방법을 알아보겠습니다. ```Java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class DateCalculator {
    public static void main(String[] args) {
        // 과거 날짜를 계산하고 싶은 날짜와 단위를 지정합니다.
        LocalDate date = LocalDate.of(2021, 10, 10);
        long monthsToSubtract = 6;

        // 지정한 날짜에서 단위를 뺀 후, 기준 날짜로 포맷을 지정하여 출력합니다.
        LocalDate pastDate = date.minus(monthsToSubtract, ChronoUnit.MONTHS);
        System.out.printf("기준 날짜로부터 %d개월 전: %s", monthsToSubtract, pastDate);
    }
}
```

출력 결과는 다음과 같습니다.
```
기준 날짜로부터 6개월 전: 2021-04-10
```

## 심층 분석
위의 예시에서는 자바 8에서 추가된 LocalDate 클래스를 활용하여 날짜를 계산하는 방법을 보여주었습니다. 이 클래스는 날짜를 나타내는 객체를 생성하고, 원하는 연산을 수행할 수 있도록 제공되는 메소드를 사용할 수 있습니다. 또한, DateTimeFormatter 클래스를 사용하여 원하는 날짜 형식으로 출력할 수 있습니다.

## 더 알아보기
- [Oracle Java Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java Tutorials - Working with Dates and Times](https://docs.oracle.com/javase/tutorial/datetime/overview/index.html)