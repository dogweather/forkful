---
date: 2024-01-20 17:37:30.776829-07:00
description: "How to: (\uBC29\uBC95) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.374389-06:00'
model: gpt-4-1106-preview
summary: .
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

## How to: (방법)
```PHP
<?php
$date = new DateTime('2023-04-01 10:00:00');
echo $date->format('Y-m-d H:i:s'); // 출력: 2023-04-01 10:00:00
echo $date->format('F j, Y, g:i a'); // 출력: April 1, 2023, 10:00 am
?>
```

```PHP
<?php
$timestamp = time();
echo date('Y-m-d H:i:s', $timestamp); // 현재 날짜와 시간 출력
?>
```

## Deep Dive (심층 탐구)
날짜 포맷팅은 PHP의 초기 버전부터 있어왔습니다. PHP 5.2.0부터는 `DateTime` 클래스와 `date_format()` 함수를 사용할 수 있게 되었죠. 이전에는 `date()` 함수와 타임스탬프를 주로 사용했습니다. `DateTime` 클래스는 날짜와 시간을 객체 지향적으로 다룰 수 있게 해주며, `IntlDateFormatter` 클래스와 같은 국제화된 날짜 형식을 지원합니다. `strftime()` 함수는 시스템의 로케일 설정에 의존하는 포맷팅을 제공하나, PHP 8.1.0 부터는 사용되지 않습니다. 선택은 상황에 따라 다를 수 있으나, 일반적으로 `DateTime` 클래스가 더 유연하고 강력합니다.

## See Also (참고 자료)
- PHP.net Date/Time Functions: https://www.php.net/manual/en/book.datetime.php
- PHP.net DateTime Class: https://www.php.net/manual/en/class.datetime.php
- PHP.net Date Formatting: https://www.php.net/manual/en/function.date.php
- PHP: The Right Way - Date and Time: https://phptherightway.com/#date_and_time
