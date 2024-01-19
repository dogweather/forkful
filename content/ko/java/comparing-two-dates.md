---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜? 

두 날짜 비교하기란 한 날짜가 다른 날짜보다 앞선지, 뒤따르는지, 아니면 동일한지를 판별하는 것을 말합니다. 이를 통해 프로그래머들은 날짜 기반의 이벤트를 일정한 순서로 정렬하거나, 두 이벤트 사이의 시간 간격을 계산할 수 있습니다.

## 어떻게:

다음은 Java에서 두 날짜를 비교하는 방법의 예입니다:

```Java
import java.time.LocalDate;

public class Main {
    public static void main(String[] args) {

        LocalDate date1 = LocalDate.of(2020, 1, 1);
        LocalDate date2 = LocalDate.of(2020, 1, 2);

        if (date1.isBefore(date2)) {
            System.out.println("date1이 date2보다 이전입니다");
        } else if (date1.isAfter(date2)) {
            System.out.println("date1이 date2보다 이후입니다");
        } else {
            System.out.println("date1과 date2는 동일합니다");
        }
    }
}
```

출력 예시:

```
date1이 date2보다 이전입니다
```

## 깊이 들어가보기:

(1) 날짜 비교는 오랜 역사를 가지고 있습니다. 처음의 컴퓨터 시스템들은 32비트 정수를 사용하여 '1970년 1월 1일'부터 초 단위로 시간을 계산했습니다.

(2) 대안으로, Java 8 이후에는 `java.time` 패키지를 사용하여 더욱 정확하고 유연하게 날짜와 시간을 비교할 수 있습니다. 

(3) `isBefore()`, `isAfter()`, `isEqual()` 메소드들은 `LocalDate` 클래스의 인스턴스에 구현되어 있으며, 이를 사용해 날짜를 비교할 수 있습니다.

## 참고 자료:

- Java `LocalDate`에 대한 자세한 설명: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- JDK의 날짜와 시간에 대한 자세한 사항: https://docs.oracle.com/javase/tutorial/datetime/