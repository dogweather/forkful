---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 필요할까?

현재 날짜를 구하는 것은 서버의 현재 날짜 및 시간을 출력하는 프로그래밍 작업입니다. 이를 통해 프로그래머들은 사용자에게 실시간 정보를 제공하거나 특정 시점에 대한 데이터를 기록하고 분석할 수 있습니다.

## 어떻게 하는가:

PHP에서 현재 날짜를 얻는 가장 간단한 방법을 살펴봅시다.

```PHP
<?php
echo date('Y-m-d H:i:s');
?>
```

이 코드는 서버의 현재 날짜와 시간을 'YYYY-MM-DD HH:MM:SS' 형식으로 출력합니다.

## 깊이 있는 정보:

PHP의 date() 함수는 1970년 1월 1일 자정 이후의 시간을 초 단위로 절대시간을 입력값으로 받아 다양한 형식의 날짜와 시간으로 변환해줍니다. 

date() 함수 외에도 DateTime 객체를 사용하여 현재 날짜와 시간을 얻을 수도 있습니다:

```PHP
<?php
$datetime = new DateTime();
echo $datetime->format('Y-m-d H:i:s');
?>
```

객체지향적 방식이 적합할 때 이 방법이 더 적합합니다. 

## 참고 자료:

1. [PHP date() 함수](https://www.php.net/manual/en/function.date.php)
2. [PHP DateTime 클래스](https://www.php.net/manual/en/class.datetime.php)
3. [PHP 날짜와 시간 설명](https://www.w3schools.com/php/php_date.asp)