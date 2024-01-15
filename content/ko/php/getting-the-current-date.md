---
title:                "현재 날짜 얻기"
html_title:           "PHP: 현재 날짜 얻기"
simple_title:         "현재 날짜 얻기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

오늘날 날짜를 알고 싶을 때가 많습니다. 예를 들어, 해당 웹사이트의 새로운 게시물을 업로드한 날짜를 알고 싶거나, E-commerce 사이트에서 주문한 제품의 배송 일정을 확인하고 싶을 때 등의 다양한 경우에 필요합니다.

## 방법

PHP에서 시간과 관련된 함수 중 하나인 "date()" 함수를 사용하여 현재 날짜를 얻을 수 있습니다.

```
PHP
$date = date('Y-m-d'); // 출력: 2021-10-13
```

위의 예시에서는 현재 날짜를 년-월-일 형식으로 출력할 수 있습니다. 여러분은 원하는 형식에 따라 다양한 방법으로 날짜를 출력할 수 있습니다.

```
PHP
$date = date('l, F jS, Y'); // 출력: Wednesday, October 13th, 2021
```

또는 시간까지 포함해서 다른 형식으로 출력할 수 있습니다.

```
PHP
$date = date('Y-m-d H:i:s'); // 출력: 2021-10-13 21:30:00
```

위의 예시 코드에서 "date('')" 안에 들어가는 포맷 문자열은 현재 날짜를 어떻게 표시할지 결정합니다. 대문자 Y는 4자리 연도를, 소문자 m은 숫자로 된 월을, 대문자 d는 2자리 숫자로 된 일을, 소문자 l은 날짜를, 대문자 F는 월의 이름을, 소문자 j는 일의 숫자를, 대문자 H는 24시간 형식으로 표시된 시간을, 소문자 i는 분을, 대문자 s는 초를 나타냅니다.

## 심층 분석

PHP의 "date()" 함수는 내부적으로 시스템 시간 설정을 기반으로 현재 날짜와 시간을 반환합니다. 이 함수는 또한 시간대와 로캘 설정을 고려하여 날짜와 시간을 반환하기 때문에 여러분이 서버에 위치한 지역 시간대를 따르게 됩니다.

"date()" 함수의 뒤에 오는 포맷 문자열은 PHP에서 지원하는 여러 날짜와 시간 표시 형식을 아우르고 있습니다. 포맷 문자열은 다양한 조합으로 사용할 수 있고, 이를 통해 다양한 형식의 날짜와 시간을 출력할 수 있습니다. 따라서 "date()" 함수를 이용하면 여러분이 원하는 형식으로 날짜와 시간을 쉽게 출력할 수 있습니다.

## 관련 링크

- [PHP 공식 문서 - date() 함수](https://www.php.net/manual/en/function.date.php)
- [PHP 날짜와 시간 다루기 - W3Schools](https://www.w3schools.com/php/php_date.asp)
- [PHP 시간대 설정하기 - TutorialsTeacher](https://www.tutorialsteacher.com/php/php-timezone-using-date-function)