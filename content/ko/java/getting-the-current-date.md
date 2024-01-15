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

## 왜?

현재 날짜를 가져오는 것이 왜 중요한지 궁금하신가요? 그렇다면 지금부터 Java에서 현재 날짜를 가져오는 방법에 대해 알려드리겠습니다.

## 어떻게 가져올까?

우선, `java.time` 패키지에서 제공하는 `LocalDate` 클래스를 import 해주세요. 그리고 `now()` 메소드를 사용해서 현재 날짜를 가져올 수 있습니다.

```Java
import java.time.LocalDate;

LocalDate currentDate = LocalDate.now();
System.out.println(currentDate);
```

위 코드를 실행하면 현재 날짜가 나타납니다.

`now()` 메소드는 기본적으로 현재 시스템 시간을 기준으로 날짜를 가져옵니다. 만약 다른 시간대를 사용하고 싶다면 `ZoneId` 클래스를 이용해 원하는 시간대를 설정할 수 있습니다.

```Java
import java.time.LocalDate;
import java.time.ZoneId;

LocalDate currentDate = LocalDate.now(ZoneId.of("Asia/Seoul"));
System.out.println(currentDate);
```

## 더 깊게 알아보기

`LocalDate` 클래스는 월, 일, 연도와 같은 날짜 정보를 담고 있습니다. 만약 시간 정보까지 함께 가져오고 싶다면 `LocalDateTime` 클래스를 사용할 수 있습니다. 또한 `LocalDate`나 `LocalDateTime` 클래스를 이용해 날짜 계산을 할 수도 있습니다.

더 자세한 내용은 [Oracle 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)에서 확인할 수 있습니다.

## 더 찾아보기

[Java Programmers를 위한 8 가지 유용한 팁](https://dzone.com/articles/7-tips-for-java-programmers)  
[Java의 Date와 Time API 소개](https://docs.oracle.com/javase/tutorial/datetime/overview/index.html)