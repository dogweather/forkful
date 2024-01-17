---
title:                "현재 날짜 가져오기"
html_title:           "PHP: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

Getting the Current Date in PHP: 현재 날짜를 PHP로 얻는 방법

## What & Why?:

현재 날짜를 받는 것은 간단하지만 매우 중요합니다. 프로그래머는 자주 현재 날짜를 필요로 하는데, 예를 들어 특정 이벤트 및 기간 계산을 위해 사용할 수 있습니다. 또한 로그파일을 생성하거나 파일에 날짜를 추가하는 데에도 사용됩니다. 

## How to:

PHP에서 현재 날짜를 얻는 것은 매우 쉽습니다. 그냥 `date` 함수를 사용하면 됩니다. 아래에 예제를 살펴보겠습니다.

```PHP
<?php
  echo date('Y-m-d'); // 출력: 2021-01-01
  echo date('l'); // 출력: Friday (오늘 요일)
```

위의 예제에서 `Y`는 년도, `m`은 월, `d`는 일, `l`은 요일을 나타냅니다. 더 많은 포맷 옵션은 PHP 공식 문서에서 확인할 수 있습니다.

## Deep Dive:

PHP에서 현재 날짜를 얻는 방법은 `date` 함수를 사용하는 것이 가장 간단합니다. 그러나 이전 버전의 PHP에서는 `time` 함수를 사용하기도 했는데, 이 함수는 Unix의 시간을 초 단위로 반환합니다. 그리고 `date` 함수는 해당 시간을 사람이 알아볼 수 있는 형식으로 변환해주는 역할을 합니다.

`DateTime` 클래스를 사용하여 현재 날짜를 가져오는 것도 가능합니다. 이는 `date` 함수보다 좀 더 객체지향적이고 더 유연한 방법입니다. 

## See Also:

- PHP `date` 함수 문서: https://www.php.net/manual/en/function.date.php
- PHP `DateTime` 클래스 문서: https://www.php.net/manual/en/class.datetime.php