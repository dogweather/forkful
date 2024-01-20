---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇이며 왜필요한가?
현재 날짜를 얻는 것은 시스템 전역 파라미터로, 시간 기반 기능을 대상으로 대상 날짜를 설정하는 등의 작업을 수행하는 데 사용됩니다. 이는 로깅이나 시간 기반의 연산처럼 시간 정보가 필요한 작업을 수행하는데 유용합니다.

## 어떻게 사용하는가:
Java에서 현재 날짜를 얻는 가장 간단한 방법은 `java.util.Date` 클래스를 사용하는 것입니다:

```Java
import java.util.Date;

// 현재 날짜 및 시간 얻기
Date date = new Date();

System.out.println(date);
```

이 코드를 실행하면 현재 날짜와 시간을 출력할 것입니다. 출력 예는 다음과 같습니다:

```
Sat Aug 22 16:52:15 KST 2022
```

## 깊이 들여다보기
Java가 탄생한 후의 초기 버전들에서는 `java.util.Date` 클래스가 유일한 옵션이었습니다. 그러나 API의 많은 문제점들 때문에 나중에 개발자들은 보다 진보된 날짜 및 시간 처리를 위해 `Java 8`에서 `java.time` 패키지를 도입하였습니다.

다음은 `java.time.LocalDate` 클래스를 사용하여 현재 날짜를 얻는 예입니다:

```Java
import java.time.LocalDate;

// 현재 날짜 얻기
LocalDate currentDate = LocalDate.now();

System.out.println(currentDate);
```

`LocalDate`에는 날짜 정보만 포함되어 있기 때문에, 시간 정보를 포함하려면 `LocalDateTime` 또는 `ZonedDateTime`을 사용하면 됩니다. 이런 방법으로는 시간대를 고려할 수 있으며, 유연한 날짜 및 시간 조작이 가능합니다.

## 참고 사항
더 자세한 정보를 얻고 싶으시다면 주어진 링크를 참조하세요:

1. Oracle 공식 자바 문서: java.util.Date - https://docs.oracle.com/javase/8/docs/api/java/util/Date.html
2. Oracle 공식 자바 문서: java.time.LocalDate - https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
3. Oracle 공식 자바 문서: java.time.LocalDateTime - https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html
4. Oracle 공식 자바 문서: java.time.ZonedDateTime - https://docs.oracle.com/javase/8/docs/api/java/time/ZonedDateTime.html