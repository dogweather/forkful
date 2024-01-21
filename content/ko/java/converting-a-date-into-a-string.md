---
title:                "날짜를 문자열로 변환하기"
date:                  2024-01-20T17:37:48.321703-07:00
model:                 gpt-4-1106-preview
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜를 문자열로 변환한다는 것은, Date 객체를 읽기 쉬운 텍스트 형식으로 바꾸는 행위입니다. 대개 날짜를 로그에 기록하거나 사용자 인터페이스에 보여주기 위해 변환합니다.

## How to: (방법)
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateToStringExample {
    public static void main(String[] args) {
        // 현재 날짜
        LocalDate currentDate = LocalDate.now();
        // 기본 형식
        String defaultFormattedDate = currentDate.toString();
        System.out.println(defaultFormattedDate); // 예: 2023-04-05

        // 사용자 정의 형식
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy년 MM월 dd일");
        String customFormattedDate = currentDate.format(formatter);
        System.out.println(customFormattedDate); // 예: 2023년 04월 05일
    }
}
```

## Deep Dive (심층 분석)
Java에서 날짜를 문자열로 변환하기 위한 두 가지 주요 클래스가 있습니다: `java.util.Date`와 `java.time.LocalDate`. `java.util.Date`는 Java의 초기 버전에서부터 사용되어 왔으나, 시간대 처리 등 여러 문제로 인해 자바 8부터는 `java.time` 패키지가 선호됩니다.

`java.text.SimpleDateFormat`는 오래된 방식으로, `java.util.Date`와 함께 사용됩니다. 하지만, 새로운 `java.time.format.DateTimeFormatter`가 그 자리를 대체했습니다. `DateTimeFormatter`는 불변이고 스레드-세이프하여, 멀티 스레드 환경에서도 안전하게 사용할 수 있습니다.

`DateTimeFormatter`를 사용할 때 `ofPattern` 메서드로 원하는 형식을 정의할 수 있습니다. 예를 들어, "yyyy년 MM월 dd일" 패턴은 년도, 월, 그리고 일을 한국어로 표시합니다.

결론적으로, 날짜 변환은 익숙해지면 간단하지만, 각 클래스와 메서드의 차이점을 이해하는 것이 중요합니다.

## See Also (추가 자료)
- [Java 8 Date/Time guide](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
- [DateTimeFormatter documentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)