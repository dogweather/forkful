---
title:                "문자열에서 날짜 분석하기"
aliases:
- ko/php/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:37.537457-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열에서 날짜 분석하기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇과 왜?

PHP에서 문자열에서 날짜 파싱하기는 날짜 및/또는 시간을 표현하는 텍스트를 PHP `DateTime` 객체나 다른 날짜/시간 형식으로 변환하는 작업을 말합니다. 이는 사용자 입력이나 외부 소스에서 온 데이터를 다룰 때, 데이터 유효성 검사, 조작, 저장 및 표시를 위해 필수적입니다.

## 방법:

PHP의 내장 `DateTime` 클래스는 날짜를 파싱하고 다루기 위한 강력한 함수 세트를 제공합니다. 생성자를 사용하여 날짜 문자열로부터 `DateTime` 인스턴스를 생성한 후, 필요에 따라 포맷할 수 있습니다. 방법은 다음과 같습니다:

```php
$dateString = "2023-04-25 15:30:00";
$dateObject = new DateTime($dateString);

echo $dateObject->format('Y-m-d H:i:s');
// 출력: 2023-04-25 15:30:00
```

비표준 형식을 따르는 문자열을 처리하려면, 입력 날짜의 정확한 형식을 지정할 수 있게 해주는 `createFromFormat` 메서드를 사용할 수 있습니다:

```php
$dateString = "25-04-2023 3:30 PM";
$dateObject = DateTime::createFromFormat('d-m-Y g:i A', $dateString);

echo $dateObject->format('Y-m-d H:i:s');
// 출력: 2023-04-25 15:30:00
```

`DateTime`이 직접 지원하지 않는 더 복잡한 파싱이 필요할 때, PHP는 영어로 된 모든 텍스트형 날짜 설명을 Unix 타임스탬프로 파싱하려는 시도하는 `strtotime` 함수를 제공합니다:

```php
$timestamp = strtotime("next Thursday");
echo date('Y-m-d', $timestamp);
// 출력은 현재 날짜에 따라 다를 것입니다, 예: "2023-05-04"
```

**타사 라이브러리 사용하기:**

PHP의 내장 함수가 다양한 사용 사례를 커버하긴 하지만, 때로는 더 정교한 파싱 능력이 필요할 수 있습니다. PHP의 DateTime 클래스를 확장한 Carbon 라이브러리는 날짜/시간 조작을 위한 풍부한 기능 세트를 제공합니다:

```php
require 'vendor/autoload.php';

use Carbon\Carbon;

$dateString = "Tomorrow";
$date = Carbon::parse($dateString);

echo $date->toDateTimeString();
// 출력은 변할 수 있습니다, 예: "2023-04-26 00:00:00"
```

Carbon의 `parse` 메서드는 다양한 날짜 및 시간 형식을 똑똑하게 처리할 수 있어, 유연한 날짜 파싱 기능이 필요한 애플리케이션에 가치 있는 도구가 됩니다.
