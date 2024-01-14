---
title:                "Java: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환할 이유는 여러 가지가 있습니다. 기본적으로 프로그램에서 날짜를 처리해야하는 경우가 있고, 데이터베이스에서 날짜를 저장하거나 검색하는 경우도 있을 수 있습니다. 이러한 경우에는 날짜를 문자열로 변환하는 작업이 필요합니다.

## 하나하나 배워보기

```Java
// Date 객체 생성
Date date = new Date();

// SimpleDateFormat 객체 생성
SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");

// 날짜를 문자열로 변환
String strDate = formatter.format(date);

// 출력
System.out.println(strDate);
```

위의 예제 코드에서는 먼저 `Date` 객체를 생성하고, 날짜를 원하는 형식으로 변환하기 위해 `SimpleDateFormat` 객체를 생성합니다. 그리고 `format()` 메소드를 사용하여 날짜를 문자열로 변환하고 출력합니다.

출력결과:
```
2020-06-18
```

위 코드에서는 `yyyy-MM-dd` 형식을 사용하였지만, 다른 형식으로도 변환할 수 있습니다. 예를 들어 `SimpleDateFormat` 객체를 생성할 때, `"yyyy/MM/dd"`, `"MM/dd/yyyy"`와 같은 다른 형식을 사용할 수 있습니다.

## 깊게 들어가기

자바에서는 `java.util.Date` 클래스를 사용하여 날짜 데이터를 다룰 수 있습니다. 이 클래스는 시간 단위까지 정확하게 표현할 수 있습니다. 하지만 그 자체로는 원하는 형식으로 날짜를 출력할 수 없기 때문에 `SimpleDateFormat` 클래스를 함께 사용하는 것이 중요합니다.

그리고 `SimpleDateFormat` 클래스의 `format()` 메소드는 `Date` 객체 뿐만 아니라 다양한 형식의 날짜 데이터를 문자열로 변환할 수 있습니다. 또한 `parse()` 메소드를 사용하면 문자열을 날짜 객체로 변환할 수도 있습니다.

## 더 자세한 정보

`Date` 클래스와 `SimpleDateFormat` 클래스에 대한 더 자세한 정보는 아래 링크를 참고하시기 바랍니다.

- [Java Date 클래스의 사용 방법 및 예제 코드](https://opentutorials.org/module/904/6749)
- [Java SimpleDateFormat 클래스의 사용 방법 및 예제 코드](https://cheese10yun.github.io/simpledateformat/)

## 다음은 무엇을 더 알아볼까요?

- [Java에서 날짜를 다루는 다른 방법들](https://opentutorials.org/module/1335/8811)
- [자바에서 문자열 포맷팅하기](http://www.tcpschool.com/java/java_api_string_formatting)