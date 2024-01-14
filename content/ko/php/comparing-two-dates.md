---
title:    "PHP: 두 날짜 비교하기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜를 비교하는 것은 웹 개발에서 일상적인 작업 중 하나입니다. 두 날짜를 비교하여 어떤 일이 발생하는지 알아보는 것은 개발자에게 중요한 스킬입니다.

## 어떻게

날짜를 비교하는 가장 간단한 방법은 PHP의 내장 함수 중 하나인 `strtotime()`를 사용하는 것입니다. 이 함수는 문자열로 표현된 날짜를 Unix 타임스탬프로 변환해 줍니다. 다음은 두 날짜를 비교하는 예제 코드입니다.

```PHP
<?php
date_default_timezone_set('Asia/Seoul');

$date1 = "2020-01-01";
$date2 = "2020-12-31";

$date1_timestamp = strtotime($date1);
$date2_timestamp = strtotime($date2);

if ($date1_timestamp < $date2_timestamp) {
    echo "두 번째 날짜가 첫 번째 날짜보다 더 늦은 날짜입니다.";
} elseif ($date1_timestamp > $date2_timestamp) {
    echo "첫 번째 날짜가 두 번째 날짜보다 더 늦은 날짜입니다.";
} else {
    echo "두 날짜는 같은 날짜입니다.";
}
```

위 코드의 출력 결과는 다음과 같습니다.

```
두 번째 날짜가 첫 번째 날짜보다 더 늦은 날짜입니다.
```

## 딥 다이브

날짜를 비교할 때 유의해야 할 점은 날짜의 형식과 시간대입니다. 위 예제에서는 두 날짜 모두 `YYYY-MM-DD` 형식이고, 시간대도 동일하게 설정해 줬습니다. 그러나 만약 날짜의 형식이 다른 경우에는 `date()` 함수를 사용하여 특정 형식으로 포맷팅해야 합니다. 또한 서로 다른 시간대의 날짜를 비교할 때에는 `DateTime` 객체를 사용하는 것이 더 정확합니다.

## 더보기

- [PHP 공식 문서 - strtotime() 함수](https://www.php.net/manual/en/function.strtotime.php)
- [PHP 공식 문서 - date() 함수](https://www.php.net/manual/en/function.date.php)
- [PHP 공식 문서 - DateTime 클래스](https://www.php.net/manual/en/class.datetime.php)