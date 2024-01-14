---
title:    "PHP: 날짜를 문자열로 변환하기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜
PHP 프로그래밍을 하는 사람들은 종종 날짜를 문자열로 변환해야할 필요가 있습니다.

## 어떻게
PHP에서 날짜를 문자열로 변환하는 방법은 다양합니다. 가장 일반적인 방법은 `date()` 함수를 사용하는 것입니다. 아래의 코드 블록을 참고해주세요.

```PHP
<?php
$date = new DateTime();
echo $date->format('Y-m-d'); // 2020-07-20
```

코드를 실행하면 위의 예시처럼 오늘의 날짜가 출력됩니다. `format()` 메소드의 인자를 바꾸면 다양한 문자열 형태의 날짜를 출력할 수 있습니다. 예를 들어, `Y-m-d H:i:s`를 인자로 넘기면 날짜와 시간이 함께 출력됩니다.

```PHP
<?php
$date = new DateTime();
echo $date->format('Y-m-d H:i:s'); // 2020-07-20 08:30:20
```

또 다른 방법으로는 `strftime()` 함수를 사용하는 것입니다. 이 함수는 지정된 포맷에 따라 로케일 설정에 따라 날짜를 출력합니다.

```PHP
<?php
echo strftime('%Y년 %m월 %d일', strtotime('now')); // 2020년 7월 20일
```

따라서 개발자가 필요에 따라 적절한 방식을 선택하여 날짜를 변환할 수 있습니다.

## 딥 다이브
날짜를 문자열로 변환하는 내부적인 동작에 대해 알고 싶다면 `strtotime()` 함수를 살펴봐야 합니다. 이 함수는 문자열 형태의 날짜를 이해할 수 있도록 unix timestamp로 변환합니다. 그리고 `date()` 함수나 `strftime()` 함수는 이 unix timestamp를 다시 형식에 맞게 변환하여 출력합니다.

이렇게 문자열을 변환하는 작업은 다소 복잡할 수 있지만 PHP에서는 다양한 방법을 제공하기 때문에 원하는 형태로 날짜를 출력할 수 있습니다.

## 참고
- [PHP 공식 문서: Date/Time 함수](https://www.php.net/manual/en/ref.datetime.php)
- [GeeksforGeeks: PHP 날짜와 시간](https://www.geeksforgeeks.org/php-date-and-time/)
- [TutorialsPoint: PHP 문자열로 변환하기](https://www.tutorialspoint.com/php/php_string_to_date.htm)