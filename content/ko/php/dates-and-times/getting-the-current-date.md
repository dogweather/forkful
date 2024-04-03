---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:31.430929-07:00
description: "PHP\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC624\uB294\
  \ \uAC83\uC740 \uC2DC\uC2A4\uD15C\uC758 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uAC80\
  \uC0C9\uD558\uACE0 \uC870\uC791\uD560 \uC218 \uC788\uAC8C \uD574\uC8FC\uB294 \uAE30\
  \uBCF8\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uC774\uB294 \uB85C\uAE45, \uD0C0\
  \uC784\uC2A4\uD0EC\uD504 \uC791\uC131, \uC774\uBCA4\uD2B8 \uC2A4\uCF00\uC904\uB9C1\
  \ \uB610\uB294 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uC2DC\uAC04\uC5D0\
  \ \uBBFC\uAC10\uD55C \uC791\uC5C5\uC744 \uC218\uD589\uD558\uB294 \uAC83\uACFC \uAC19\
  \uC740 \uAE30\uB2A5\uC5D0 \uC788\uC5B4 \uC911\uC694\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.372763-06:00'
model: gpt-4-0125-preview
summary: "PHP\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC624\uB294\
  \ \uAC83\uC740 \uC2DC\uC2A4\uD15C\uC758 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uAC80\
  \uC0C9\uD558\uACE0 \uC870\uC791\uD560 \uC218 \uC788\uAC8C \uD574\uC8FC\uB294 \uAE30\
  \uBCF8\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

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
