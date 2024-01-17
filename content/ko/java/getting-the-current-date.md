---
title:                "현재 날짜 가져오기"
html_title:           "Java: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

현재 날짜를 가져오는 것은 프로그래머에게 중요한 작업입니다. 현재 날짜를 가져오는 이유는 프로그램이 실행되는 날짜와 시간을 기록하거나 프로그램의 동작을 제어하기 위해 필요할 수 있기 때문입니다.

## 하는 방법:

자바에서 현재 날짜를 가져오는 가장 간단한 방법은 java.util 패키지에 있는 Date 클래스를 사용하는 것입니다. 아래의 코드를 참조해보세요.

```java
import java.util.Date;

public class CurrentDate{
    public static void main(String[] args) {
        // 현재 날짜를 가져오는 Date 객체 생성
        Date currentDate = new Date();

        // 현재 날짜 출력
        System.out.println("현재 날짜: " + currentDate);
    }
}
```

위의 코드를 실행하면 아래와 같은 결과가 출력될 것입니다.

```
현재 날짜: Sat Aug 21 14:38:45 KST 2021
```

## 깊이 있는 정보:

현재 날짜를 가져오는 작업은 Java 1.1 이후로 추가된 Date 클래스의 메소드를 이용해 수행됩니다. 또 다른 방법으로는 java.time 패키지에 있는 LocalDate 클래스를 사용하는 것도 가능합니다. 하지만 Date 클래스는 날짜와 시간 모두를 제공하고, LocalDate 클래스는 오직 날짜만을 처리하기 때문에 용도에 맞게 선택하여 사용하시면 됩니다. 또한, JAVA 언어 이외에도 다른 프로그래밍 언어에서도 현재 날짜를 가져오는 메소드를 지원하고 있으니 필요에 따라 다른 방법도 채택해보세요.

## 더 알아보기:

현재 날짜를 가져오는 작업과 관련된 더 많은 정보를 알고 싶다면 아래의 링크를 참조해보세요.

- Java Date 클래스: https://docs.oracle.com/javase/7/docs/api/java/util/Date.html
- Java LocalDate 클래스: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- 다른 프로그래밍 언어에서 현재 날짜를 가져오는 방법: https://www.tutorialspoint.com/how-to-get-current-date-and-time-in-different-programming-languages