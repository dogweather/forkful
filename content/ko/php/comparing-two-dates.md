---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
두 날짜를 비교하는 것은 며칠, 몇 시간, 몇 분, 혹은 몇 초와 같이 두 시점 사이의 시간 차이를 계산하는 것입니다. 이를 통해 프로그래머들은 일정 관리, 이벤트 기간 결정, 데이터 분석 등 다양한 코딩 문제를 해결할 수 있습니다.

## 어떻게 할까? 
```PHP
<?php
$date1=date_create('2023-03-15'); // 날짜 객체 생성
$date2=date_create('2023-03-21');
$diff=date_diff($date1,$date2); // 두 날짜를 비교

echo $diff->format('%R%a days'); // 차이 출력
?>
```
위 코드를 실행하면, 결과로 "+6 days"가 출력됩니다. 

## Deep Dive
PHP에서 두 날짜를 비교하려면 우선 `date_create` 함수로 날짜 객체를 생성합니다. 이 역시 PHP 5.2.0 버전부터 제공됩니다. 또한, `date_diff` 함수를 이용해 두 날짜의 차이를 계산하는데, 이는 PHP 5.3.0부터 사용 가능합니다.

또한, `date_diff` 함수를 사용하는 대신 `strtotime` 함수를 사용하여 두 날짜를 UNIX 타임스탬프로 변환한 후, 이를 직접 비교하는 방법도 있습니다. 하지만 이 방법은 두 날짜가 매우 멀 때 오차가 발생할 수 있습니다.

## 참고하기 
* PHP 날짜와 시간 함수에 대해 자세히 알고 싶다면, PHP 공식 문서를 참고하세요. (https://www.php.net/manual/en/book.datetime.php)
* 또한 PHP에서 두 날짜를 비교하는 여러 방법에 대해서는 여기를 확인해보세요. (https://www.php.net/manual/en/datetime.diff.php)