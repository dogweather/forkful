---
title:                "Java: 현재 날짜 가져오기"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 현재 날짜를 가져오는 이유

Java 프로그래밍을 하는 많은 사람들은 현재 날짜에 대한 정보를 필요로 합니다. 예를 들어, 파일 이름에 날짜를 추가하거나 특정 이벤트나 기록을 남기기 위해 현재 날짜를 사용할 수 있습니다. 또한, 날짜를 비교하거나 분석하는 프로그램을 작성할 때도 현재 날짜가 필요합니다.

# 가져오는 방법

자바에서 현재 날짜를 가져오는 방법은 간단합니다. Date 라이브러리를 사용하면 됩니다. 다음은 자바 코드로 현재 날짜를 가져오는 예시입니다.

```Java
import java.util.Date;

public class CurrentDate {
    public static void main(String[] args) {
        // 현재 날짜를 가져오는 Date 객체 생성
        Date currentDate = new Date();
        // 현재 날짜를 출력
        System.out.println("Current Date: " + currentDate);
    }
}
```

위의 코드를 실행하면 다음과 같은 결과가 출력됩니다.

```
Current Date: Mon Oct 18 14:10:05 KST 2021
```

# 깊이 파헤치기

현재 날짜를 가져오는 방법은 실제로 Date 객체 생성 후 시간을 설정하는 것입니다. Date 객체는 기본적으로 현재 시간으로 초기화되며, getTime() 메소드를 사용하여 밀리초 단위로 시간을 가져올 수 있습니다. 또한, Date 객체를 원하는 형식으로 출력하려면 SimpleDateFormat 클래스를 사용하여 형식을 지정할 수 있습니다.

# 관련 자료

- [Java Date 클래스 API 문서](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [SimpleDateFormat 클래스를 이용한 날짜 출력 예시](https://www.javatpoint.com/java-date-format)
- [자바로 날짜와 시간 다루는 방법](https://www.baeldung.com/java-date-time)