---
title:                "PHP: 날짜를 문자열로 변환하기"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜
왜 날짜를 문자열로 변환하는 것에 참여해야 할 지 알아봅시다.

## 하는 법
```PHP
// 현재 날짜 설정
$date = date('Y-m-d');

// 옵션을 사용하여 원하는 날짜 형식으로 변환
$date_string = date('F j, Y', strtotime($date));

// 결과 출력
echo '오늘 날짜는' . $date_string . '입니다.';
```

**출력:** 오늘 날짜는 January 1, 2021입니다.

## 깊은 곳으로
날짜를 문자열로 변환하는 것은 데이터 시스템에서 정확한 날짜를 표현하는 데 중요합니다. PHP의 `date()` 함수를 사용하면 여러 옵션을 사용하여 원하는 형식으로 날짜를 형성할 수 있습니다. `strtotime()` 함수를 사용하면 문자열 형식의 날짜를 시간 값으로 변환할 수 있습니다.

## 참고
- [PHP 공식 문서 - `date()` 함수](https://www.php.net/manual/en/function.date.php)
- [PHP 공식 문서 - `strtotime()` 함수](https://www.php.net/manual/en/function.strtotime.php)