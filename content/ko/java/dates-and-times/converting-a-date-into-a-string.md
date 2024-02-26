---
date: 2024-01-20 17:37:48.321703-07:00
description: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD55C\uB2E4\
  \uB294 \uAC83\uC740, Date \uAC1D\uCCB4\uB97C \uC77D\uAE30 \uC26C\uC6B4 \uD14D\uC2A4\
  \uD2B8 \uD615\uC2DD\uC73C\uB85C \uBC14\uAFB8\uB294 \uD589\uC704\uC785\uB2C8\uB2E4\
  . \uB300\uAC1C \uB0A0\uC9DC\uB97C \uB85C\uADF8\uC5D0 \uAE30\uB85D\uD558\uAC70\uB098\
  \ \uC0AC\uC6A9\uC790 \uC778\uD130\uD398\uC774\uC2A4\uC5D0 \uBCF4\uC5EC\uC8FC\uAE30\
  \ \uC704\uD574 \uBCC0\uD658\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-02-25T18:49:52.068005-07:00'
model: gpt-4-1106-preview
summary: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD55C\uB2E4\uB294\
  \ \uAC83\uC740, Date \uAC1D\uCCB4\uB97C \uC77D\uAE30 \uC26C\uC6B4 \uD14D\uC2A4\uD2B8\
  \ \uD615\uC2DD\uC73C\uB85C \uBC14\uAFB8\uB294 \uD589\uC704\uC785\uB2C8\uB2E4. \uB300\
  \uAC1C \uB0A0\uC9DC\uB97C \uB85C\uADF8\uC5D0 \uAE30\uB85D\uD558\uAC70\uB098 \uC0AC\
  \uC6A9\uC790 \uC778\uD130\uD398\uC774\uC2A4\uC5D0 \uBCF4\uC5EC\uC8FC\uAE30 \uC704\
  \uD574 \uBCC0\uD658\uD569\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
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
