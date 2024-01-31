---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:15:00.027494-07:00
simple_title:         "현재 날짜 가져오기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
현재 날짜를 알아내는 것은 코드가 실행되는 날짜를 파악하는 데 필요하다. 이는 로깅, 문서화, 기능적인 요소에 사용되며 사용자 경험을 향상시키기 위해 주로 사용된다.

## How to: (방법)
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        // 오늘 날짜를 얻는다.
        LocalDate today = LocalDate.now();
        
        // 출력한다.
        System.out.println("오늘 날짜: " + today);
    }
}
```
출력 예시:
```
오늘 날짜: 2023-03-29
```

## Deep Dive (심층 분석)
`java.time` 패키지는 Java 8에서 도입되었다. 이전에는 `java.util.Date`나 `java.util.Calendar`가 주로 사용되었다. `LocalDate.now()`는 시스템 클록과 기본 시간대를 사용해 현재 날짜를 제공한다. `java.time.ZonedDateTime`을 사용하면 특정 시간대의 현재 날짜와 시각을 얻을 수 있다. 또한, 이 메서드들은 테스트가 어려운 정적 메소드다. 이러한 이유로 일부 프로그래머들은 테스트가 용이한 대안적인 방법을 사용하기도 한다.

## See Also (참고 자료)
- [Java 8 Date-Time API Guide](https://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html)
- [`LocalDate` JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Baeldung Guide on `java.time`](https://www.baeldung.com/java-8-date-time-intro)
