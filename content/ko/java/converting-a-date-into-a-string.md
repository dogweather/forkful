---
title:                "Java: 날짜를 문자열로 변환하기"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 방법이 중요한 이유는 다음과 같습니다. 프로그래밍에서 날짜는 매우 중요한 개념이며 많은 분야에서 사용됩니다. 따라서 특정 날짜를 적절한 문자열 형식으로 변환하는 것은 불가피합니다.

## 어떻게

이제 코드 블록을 사용하여 여러가지 방법으로 날짜를 문자열로 변환하는 방법을 살펴보겠습니다.

```java
import java.text.SimpleDateFormat;
import java.util.Date;

Date date = new Date();
SimpleDateFormat format1 = new SimpleDateFormat("yyyy/MM/dd");

// Date를 yyyy/MM/dd 형식의 문자열로 변환
String strDate1 = format1.format(date);
System.out.println("String: " + strDate1);
```
```
출력:
String: 2020/10/05
```

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

LocalDate date = LocalDate.now();
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy/MM/dd");

// LocalDate를 yyyy/MM/dd 형식의 문자열로 변환
String strDate2 = date.format(formatter);
System.out.println("String: " + strDate2);
```
```
출력:
String: 2020/10/05
```

```java
import java.util.Calendar;

Calendar calendar = Calendar.getInstance();

// java.util.Calendar를 yyyy/MM/dd 형식의 문자열로 변환
String strDate3 = String.format("%1$tY/%1$tm/%1$td", calendar);
System.out.println("String: " + strDate3);
```
```
출력:
String: 2020/10/05
```

위의 예제들은 각각 `Date`, `LocalDate`, `Calendar` 객체를 이용하여 날짜를 문자열로 변환하는 방법을 보여주고 있습니다. 각각의 클래스마다 다른 방식으로 문자열 형식을 지정해주어야 합니다. 따라서 자신이 사용하는 클래스에 따라 알맞은 방법을 선택해야 합니다.

## 더 깊이 들어가기

사실 날짜를 문자열로 변환하는 방법은 많은 다양한 방식이 있습니다. 위의 예제들은 간단한 형태의 날짜를 문자열로 변환하는 방법이지만, 더 복잡한 형태의 날짜를 다루어야 할 경우 더 많은 설정을 필요로 할 수 있습니다. 따라서 자신이 다루어야 할 날짜에 맞는 적절한 변환 방법을 찾아서 적용해야 합니다.

## 참고

* [Java - Date를 String으로 변환하기](https://helle-world.tistory.com/37)
* [Date 형식을 String으로 변환하는 방법](https://lemontia.tistory.com/886)
* [Java Calendar를 String으로 변환하는 방법](https://madplay.github.io/post/java-calendar-to-string-format)
* [SimpleDateFormat 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)