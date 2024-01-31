---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:37:38.755078-07:00
html_title:           "Arduino: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"

category:             "PHP"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 날짜를 파싱한다는 건 텍스트로 된 날짜 정보를 날짜 타입으로 변환하는 작업입니다. 이를 통해 날짜와 관련된 계산이나 비교 같은 기능을 수행할 수 있지요.

## How to: (방법)
PHP에서 `DateTime` 클래스와 `date_create_from_format()` 함수를 사용하면 문자열로부터 날짜를 쉽게 파싱할 수 있습니다. 예를 들어 봅시다.

```php
<?php

$dateString = '2023-04-01';
$date = date_create_from_format('Y-m-d', $dateString);
echo $date->format('Y-m-d'); // 출력: 2023-04-01

?>
```

또는 현재 버전의 PHP에서 `DateTime::createFromFormat` 메소드를 사용할 수도 있습니다.

```php
<?php

$dateString = '2023년 4월 1일';
$date = DateTime::createFromFormat('Y년 m월 d일', $dateString);
echo $date->format('Y-m-d'); // 출력: 2023-04-01

?>
```

## Deep Dive (심층 탐구)
PHP에서 날짜 파싱은 초기 버전부터 중요한 기능이었습니다. `strtotime()`, `DateTime` 클래스, `date_create_from_format()` 함수는 개선을 거듭하며 발전해왔죠. 사용 상황에 따라 `strtotime()`은 간단한 시나리오에 사용하기 좋고, `DateTime` 클래스와 `date_create_from_format()` 함수는 더 정교한 날짜 형식이 필요할 때 탁월합니다. `DateTime`은 객체 지향적 접근을 제공하며, 더 복잡한 날짜 연산에 유용합니다.

## See Also (관련 자료)
- [PHP: DateTime - Manual](https://www.php.net/manual/en/class.datetime.php)
- [PHP: date_create_from_format - Manual](https://www.php.net/manual/en/function.date-create-from-format.php)
- [PHP: strtotime - Manual](https://www.php.net/manual/en/function.strtotime.php)
