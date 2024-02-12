---
title:                "현재 날짜 가져오기"
aliases:
- /ko/php/getting-the-current-date/
date:                  2024-02-03T19:10:31.430929-07:00
model:                 gpt-4-0125-preview
simple_title:         "현재 날짜 가져오기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
PHP에서 현재 날짜를 가져오는 것은 시스템의 날짜와 시간을 검색하고 조작할 수 있게 해주는 기본적인 작업입니다. 이는 로깅, 타임스탬프 작성, 이벤트 스케줄링 또는 애플리케이션에서 시간에 민감한 작업을 수행하는 것과 같은 기능에 있어 중요합니다.

## 방법:
### PHP 내장 함수
PHP의 내장된 `date()` 함수는 현재 날짜를 얻는 가장 직접적인 방법입니다. 형식 매개변수를 지정하여 다양한 방식으로 날짜를 형식화할 수 있습니다.

```php
echo date("Y-m-d"); // 출력: 2023-04-01 (예시)
echo date("l, F j, Y"); // 출력: 토요일, 4월 1일, 2023
```

타임존을 지원하며 날짜와 시간을 얻기 위해서는 `DateTime` 클래스와 `DateTimeZone`을 사용할 수 있습니다.

```php
$dateTime = new DateTime('now', new DateTimeZone('America/New_York'));
echo $dateTime->format('Y-m-d H:i:s'); // 출력: 2023-04-01 12:00:00 (예시)
```

### 카본 사용하기 (인기 있는 서드파티 라이브러리)
[카본](https://carbon.nesbot.com/)은 `DateTime`에 대한 간단한 API 확장으로, 날짜와 시간을 다루는 데 있어 더욱 깔끔하고 유려한 방법을 제공합니다.

먼저 Composer를 통해 카본이 설치되어 있는지 확인합니다:
```bash
composer require nesbot/carbon
```

그런 다음, 현재 날짜를 가져오는 데 사용할 수 있습니다:

```php
use Carbon\Carbon;

echo Carbon::now(); // 출력: 2023-04-01 12:00:00 (예시, 기본 형식)
echo Carbon::now()->toDateString(); // 출력: 2023-04-01
echo Carbon::now()->format('l, F j, Y'); // 출력: 토요일, 4월 1일, 2023
```

카본은 PHP에서 날짜-시간 처리를 풍부하게 하여 시간 조작, 비교, 형식화에 대한 가독성과 다양한 기능을 추가합니다.
