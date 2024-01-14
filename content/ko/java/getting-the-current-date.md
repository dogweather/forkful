---
title:    "Java: 현재 날짜 가져오기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜: 현재 날짜를 가져오는 것이 중요한 이유

컴퓨터 프로그래밍을 할 때, 모든 프로그램은 현재 시간과 날짜에 많은 의존성이 있습니다. 예를 들어, 파일 생성일, 시간 기반 이벤트 스케줄링, 사용자 인터페이스 등에서 현재 날짜를 사용하여 프로그램의 동작을 조정할 수 있습니다. 이러한 이유로 현재 날짜를 가져오는 것은 중요합니다.

## 어떻게: 현재 날짜를 가져오는 방법

Java에서 현재 날짜를 가져오는 방법은 간단합니다. Date 클래스를 사용하여 현재 시간을 객체로 생성하고, SimpleDateFormat 클래스를 사용하여 원하는 형식으로 날짜를 출력할 수 있습니다. 아래의 예제 코드를 참고해보세요.

```Java
import java.util.Date;
import java.text.SimpleDateFormat;

public class CurrentDate {
  public static void main(String[] args) {
    // 현재 날짜 객체 생성
    Date currentDate = new Date();

    // 날짜 출력 형식 지정
    SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy년 MM월 dd일");

    // 현재 날짜를 지정한 형식으로 출력
    System.out.println("현재 날짜: " + dateFormat.format(currentDate));
  }
}
```

위의 코드를 실행하면 아래와 같은 결과가 출력됩니다.

```
현재 날짜: 2021년 01월 01일
```

## 깊이 있는 정보: 현재 날짜를 가져오는 원리

Java에서는 Date 클래스를 사용하여 현재 날짜와 시간을 객체로 생성할 수 있습니다. 이 클래스는 다양한 메소드를 제공하여 날짜와 시간을 다룰 수 있도록 도와줍니다. 또한 SimpleDateFormat 클래스를 사용하여 날짜를 지정한 형식으로 출력할 수 있으며, 이 클래스는 많은 형식 지정자를 제공하여 날짜를 원하는 형식으로 변환할 수 있도록 해줍니다.

Java 8부터는 새로운 날짜와 시간 API인 LocalDateTime 클래스를 제공합니다. 이 클래스가 제공하는 메소드를 사용하여 날짜와 시간을 더 쉽게 다룰 수 있습니다.

# 참고자료

- [Java Date 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java SimpleDateFormat 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java 8 LocalDateTime 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)