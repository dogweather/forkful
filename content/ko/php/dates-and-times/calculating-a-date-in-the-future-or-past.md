---
title:                "미래나 과거의 날짜 계산하기"
aliases:
- /ko/php/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:50.797043-07:00
model:                 gpt-4-1106-preview
simple_title:         "미래나 과거의 날짜 계산하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜 계산은 미래나 과거의 특정 날짜를 결정하는 것입니다. 프로그래머들은 이벤트 스케줄링, 기한 설정, 시간 기반 알람 등을 위해 이를 사용합니다.

## How to: (어떻게 하나요?)
PHP는 날짜 계산을 하기 위해 `DateTime` 클래스와 `DateInterval` 클래스를 제공합니다. 여기 간단한 예제들이 있습니다:

```PHP
<?php
// 오늘 날짜
$today = new DateTime();
echo $today->format('Y-m-d H:i:s'); // 예: 2023-04-15 14:20:00

// 10일 후
$futureDate = clone $today;
$futureDate->add(new DateInterval('P10D')); // P는 기간(period), D는 일(day)을 의미
echo $futureDate->format('Y-m-d H:i:s'); // 예: 2023-04-25 14:20:00

// 3주 전
$pastDate = clone $today;
$pastDate->sub(new DateInterval('P3W')); // W는 주(week)를 의미
echo $pastDate->format('Y-m-d H:i:s'); // 예: 2023-03-25 14:20:00
?>
```
## Deep Dive (심층 탐구)
`DateTime`과 `DateInterval`은 PHP 5.2.0부터 도입되었습니다. 이전에는 `strtotime()`과 `mktime()` 같은 함수들이 주로 사용되었습니다.

### 대체법
`strtotime()`는 문자열을 분석해 Unix 타임스탬프를 반환합니다. 이를 사용해 과거나 미래 날짜를 얻을 수도 있습니다:
```PHP
echo date('Y-m-d', strtotime('+10 days')); // 10일 후
```

### 구현 세부사항
시간대 처리를 위해 `DateTimeZone` 클래스를 사용할 수 있습니다. 날짜 계산 시 시간대가 중요합니다. 오류 방지를 위해 `try-catch` 블록 내에서 날짜 관련 연산을 수행하는 것이 좋습니다.

## See Also (더 보기)
- PHP의 `DateTime` 클래스 [공식 문서](https://www.php.net/manual/en/class.datetime.php)
- `DateInterval` 클래스 [공식 문서](https://www.php.net/manual/en/class.dateinterval.php)
- 시간 관련 함수 `strtotime()` [공식 문서](https://www.php.net/manual/en/function.strtotime.php)
