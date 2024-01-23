---
title:                "두 날짜 비교하기"
date:                  2024-01-20T17:33:56.746970-07:00
model:                 gpt-4-1106-preview
simple_title:         "두 날짜 비교하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜를 비교한다는 건 특정 두 시점을 대조하는 것입니다. 일정 관리, 데이터 정렬, 유효 기간 확인 같은 경우에 쓰여요.

## How to (방법)
PHP에서 날짜 비교는 `DateTime` 클래스와 `>` , `<` , `==` 연산자로 할 수 있습니다. 예제를 보죠.

```PHP
<?php
$date1 = new DateTime("2023-04-01 10:00:00");
$date2 = new DateTime("2023-04-01 15:00:00");

if ($date1 < $date2) {
    echo "date1이 date2보다 일찍입니다.";
} else if ($date1 == $date2) {
    echo "date1과 date2가 같은 시간입니다.";
} else {
    echo "date1이 date2보다 늦습니다.";
}
?>
```
출력 결과:
```
date1이 date2보다 일찍입니다.
```

## Deep Dive (깊이 알기)
초창기 PHP에서는 `strtotime()` 함수와 유닉스 타임스탬프를 사용하여 날짜를 비교했지만, `DateTime` 클래스가 도입되면서 더 직관적이고 강력한 방법을 제공합니다. `DateTime`는 시간대를 처리하고 더 복잡한 날짜 연산을 가능하게 해 줍니다. `DateTimeImmutable`는 값을 변경할 수 없는 `DateTime`의 변종으로, 변경될 위험 없이 날짜 시간을 다루길 원할 때 사용합니다. `DateInterval`과 `DatePeriod`도 있지만, 그저 비교하는 거라면 `DateTime`만으로 충분합니다.

다른 방식으로는 `DateTime::diff()` 함수를 사용해 두 날짜의 차이를 `DateInterval`로 반환하여 비교할 수도 있습니다. 이 방법은 날짜 간의 정확한 기간 차이를 알고 싶을 때 유용합니다.

```PHP
<?php
$date1 = new DateTime("2023-04-01 10:00:00");
$date2 = new DateTime("2023-04-02 10:00:00");

$interval = $date1->diff($date2);
echo $interval->format("%d days, %h hours, %i minutes");
?>
```
출력 결과:
```
1 days, 0 hours, 0 minutes
```

## See Also (더 보기)
- PHP 공식 문서의 DateTime 클래스: https://www.php.net/manual/en/class.datetime.php
- PHP 공식 문서의 DateInterval 클래스: https://www.php.net/manual/en/class.dateinterval.php
- 시간대 관리하기: https://www.php.net/manual/en/datetime.settimezone.php
