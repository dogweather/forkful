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

# 무엇 & 왜?
날짜를 미래나 과거로 계산하는 것은 프로그래머들이 일반적으로 하는 작업입니다. 이 작업의 목적은 특정 날짜를 계산하거나 특정 날짜와 관련된 데이터를 분석하기 위한 것입니다.

# 어떻게 하나요?
자바에서 미래나 과거 날짜를 계산하는 방법을 알아보겠습니다. 먼저 날짜 객체를 생성한 다음, Calendar 클래스를 사용하여 날짜를 세팅합니다. 그 후, add 메서드를 이용하여 특정 기간을 더하거나 빼주면 됩니다. 코드 예시와 출력 결과는 다음과 같습니다.

```Java
import java.util.Calendar;
import java.util.Date;

public class DateCalculator {
  public static void main(String[] args) {
    // 날짜 객체 생성
    Calendar calendar = Calendar.getInstance();
    // 현재 날짜로 세팅
    Date currentDate = new Date();
    calendar.setTime(currentDate);

    // 1년 후 계산
    calendar.add(Calendar.YEAR, 1);
    Date oneYearLater = calendar.getTime();
    System.out.println("1년 후: " + oneYearLater);

    // 2달 전 계산
    calendar.add(Calendar.MONTH, -2);
    Date twoMonthsAgo = calendar.getTime();
    System.out.println("2달 전: " + twoMonthsAgo);
  }
}
```
```
1년 후: Sun Jun 20 17:08:13 KST 2021
2달 전: Mon Feb 20 17:08:13 KST 2021
```

# 깊이 파헤치기
미래나 과거 날짜를 계산하는 방법은 프로그래머들이 필수적으로 알아야 하는 기본적인 작업 중 하나입니다. 과거에는 Date 클래스를 사용하여 날짜를 다루었지만, 이제는 Calendar 클래스를 사용하는 것이 좋습니다. 또한 LocalDate 클래스를 사용할 수도 있습니다. Calendar 클래스는 특정 시점의 날짜를 기준으로 계산을 하기 때문에, 정확한 날짜 계산에는 유용하지만, LocalDate 클래스는 시간대와 시간을 포함하지 않는 단순한 날짜 계산에 적합합니다.

# 참조
- [Oracle Java 문서: Calendar 클래스](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [공식 Java Tutorial: 날짜 계산](https://docs.oracle.com/javase/tutorial/datetime/iso/date.html)
- [Oracle Java 문서: LocalDate 클래스](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)