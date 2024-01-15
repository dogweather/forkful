---
title:                "날짜를 문자열로 변환하기"
html_title:           "Java: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것에 대해 생각해 본 적이 있나요? 자바에서는 많은 경우에 날짜를 문자열로 변환해야 할 필요가 있습니다. 예를 들어, 데이터베이스에서 날짜를 가져와서 사용자에게 보여주는 경우나, 웹 애플리케이션에서 날짜를 보여줄 때 등등 다양한 상황에서 날짜를 문자열로 변환하는 것이 유용할 수 있습니다.

## 방법

날짜를 문자열로 변환하는 방법은 간단합니다. 먼저 `DateFormat` 클래스나 `SimpleDateFormat` 클래스를 사용하여 날짜를 포맷할 형식을 지정합니다. 그리고 `format()` 메소드를 사용하여 날짜를 문자열로 변환하면 됩니다. 아래는 예시 코드와 그 결과입니다.

```java
DateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
Date date = new Date();
String dateString = dateFormat.format(date);

System.out.println(dateString);
```

출력:
```
07/01/2020
```

## 깊이 파고들기

자바에서 날짜를 문자열로 변환하는 방법은 위에서 설명한 것처럼 간단하게 사용할 수 있지만, 실제로는 조금 더 복잡한 내부 로직을 포함하고 있습니다. `DateFormat` 클래스나 `SimpleDateFormat` 클래스는 자신이 지정된 날짜 포맷에 따라 `Calendar` 객체를 사용하여 날짜를 해석합니다. 또한 일관된 국제 날짜 형식을 지원하기 위해 `Locale` 객체를 사용합니다. 따라서 다른 국가에서도 같은 날짜 포맷이 사용된다는 것을 보장할 수 있습니다.

## 관련 링크

- [Java Date and Time API 문서](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html#package.description)
- [SimpleDateFormat 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java DateFormat 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/text/DateFormat.html)