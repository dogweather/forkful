---
date: 2024-01-20 17:37:30.776829-07:00
description: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD55C\uB2E4\
  \uB294 \uAC83\uC740, PHP \uC5D0\uC11C \uD2B9\uC815\uD55C \uB0A0\uC9DC \uD615\uC2DD\
  \uC744 \uAC00\uB3C5\uC131\uC774 \uC88B\uC740 \uD14D\uC2A4\uD2B8 \uD615\uD0DC\uB85C\
  \ \uBC14\uAFB8\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uB370\uC774\uD130\uBCA0\uC774\uC2A4\uC5D0 \uC800\uC7A5, \uC0AC\
  \uC6A9\uC790 \uC778\uD130\uD398\uC774\uC2A4 \uD45C\uC2DC, \uB610\uB294 \uB2E4\uB978\
  \ \uD615\uD0DC\uC758 \uB370\uC774\uD130 \uCC98\uB9AC\uB97C \uC704\uD574 \uC774 \uC791\
  \uC5C5\uC744 \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.374389-06:00'
model: gpt-4-1106-preview
summary: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD55C\uB2E4\uB294\
  \ \uAC83\uC740, PHP \uC5D0\uC11C \uD2B9\uC815\uD55C \uB0A0\uC9DC \uD615\uC2DD\uC744\
  \ \uAC00\uB3C5\uC131\uC774 \uC88B\uC740 \uD14D\uC2A4\uD2B8 \uD615\uD0DC\uB85C \uBC14\
  \uAFB8\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uB370\uC774\uD130\uBCA0\uC774\uC2A4\uC5D0 \uC800\uC7A5, \uC0AC\uC6A9\
  \uC790 \uC778\uD130\uD398\uC774\uC2A4 \uD45C\uC2DC, \uB610\uB294 \uB2E4\uB978 \uD615\
  \uD0DC\uC758 \uB370\uC774\uD130 \uCC98\uB9AC\uB97C \uC704\uD574 \uC774 \uC791\uC5C5\
  \uC744 \uD569\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
날짜를 문자열로 변환한다는 것은, PHP 에서 특정한 날짜 형식을 가독성이 좋은 텍스트 형태로 바꾸는 것을 말합니다. 프로그래머들은 데이터베이스에 저장, 사용자 인터페이스 표시, 또는 다른 형태의 데이터 처리를 위해 이 작업을 합니다.

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
