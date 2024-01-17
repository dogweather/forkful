---
title:                "날짜를 문자열로 변환하기"
html_title:           "PHP: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

날짜를 문자열로 변환하는 것은 실제 문자열이 아니지만 날짜 데이터를 문자열 형태로 표현하는 것을 말합니다. 프로그래머들은 이를 하는 이유는 데이터를 다루기 쉽고 이해하기 쉽게끔 변환하기 위해서 입니다.

## 방법:

```PHP 
echo date('Ymd'); // 오늘 날짜를 문자열로 변환하여 출력 
// Output: 20211225
```

```PHP
$date = '15/04/2021'; // 지정된 날짜 데이터 
echo date('Y-m-d', strtotime($date)); // 날짜 데이터를 원하는 형식의 문자열로 변환하여 출력 
// Output: 2021-04-15
```

## 깊은 곳:

- 날짜를 문자열로 변환하기 전에는 날짜를 Unix timestamp 형태로 다루었습니다.
- PHP에서는 ``` date() ``` 함수를 사용하여 날짜 형식을 지정할 수 있습니다.
- 날짜를 문자열로 변환하는 또 다른 방법으로는 ``` DateTime ``` 클래스를 사용하는 것이 있습니다.

## 관련 자료:

- [PHP 공식 문서: date() 함수](https://www.php.net/manual/en/function.date.php)
- [PHP 공식 문서: DateTime 클래스](https://www.php.net/manual/en/class.datetime.php)
- [W3Schools: PHP Date and Time Functions](https://www.w3schools.com/php/php_date.asp)