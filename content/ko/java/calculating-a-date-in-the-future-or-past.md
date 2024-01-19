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

## 무엇이며 왜 필요한가?
미래나 과거의 날짜를 계산하는 것은 특정 기간 후의 날짜를 찾거나 특정 날짜로부터 뒤로 가거나 앞으로 오는 날짜를 확인하는 것을 의미합니다. 프로그래머들이 이를 수행하는 주된 이유는 예약, 스케줄링, 기간 내 이벤트 관리 등을 위해 시간을 추적하고 조정하는 상황이 자주 발생하기 때문입니다.

## 어떻게 하는가:
Java의 `java.time` 패키지를 사용하면 쉽게 날짜 계산을 수행할 수 있습니다.

```Java
import java.time.LocalDate;
import java.time.Period;

public class Main {
   public static void main(String args[]) {
      LocalDate today = LocalDate.now();
      Period oneWeek = Period.ofWeeks(1);
      LocalDate nextWeek = today.plus(oneWeek);
      LocalDate lastWeek = today.minus(oneWeek);
      
      System.out.println("오늘: " + today);
      System.out.println("다음 주: " + nextWeek);
      System.out.println("지난 주: " + lastWeek);
   }
}
```

출력:

```Java
오늘: 2022-09-30
다음 주: 2022-10-07
지난 주: 2022-09-23
```

## 깊게 알아보기
날짜 계산은 컴퓨터 과학의 초기부터 주요 문제 중 하나였습니다. Java 예제에서 볼 수 있듯이, 최근의 언어들은 날짜를 핸들링하기 위한 복잡한 세부 사항을 추상화하는 강력한 API를 포함하고 있습니다.

이러한 계산에는 여러 대안이 있습니다. 일반적인 대안 중 하나는 Java의 `java.util.Calendar` 클래스를 사용하는 것입니다. 다른 어떤 언어들에서는 직접 '연-월-일' 시간 단위를 증가시키고 감소시키는 비교적 기본적인 방법을 사용하기도 합니다.

날짜 계산의 구현 세부사항은 다양하며, 흔히 무시될 수 있는 복잡성을 가지고 있는 경우가 많습니다. 이러한 복잡성에는 윤년, 표준 시간대, 서머타임 등이 포함됩니다.

## 참고 자료
1. Oracle 공식 Java 문서의 'java.time' 패키지: [링크](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html)
2. Java의 날짜와 시간 API에 대한 자세한 가이드: [링크](https://www.baeldung.com/java-8-date-time-intro)
3. 윤년 및 시간대 계산에 대한 추가 정보: [링크](http://www.timeanddate.com)