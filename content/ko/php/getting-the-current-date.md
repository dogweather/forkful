---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:15:44.953941-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
PHP에서 현재 날짜를 얻는 것은 시스템의 지금 시각을 반환하는 것입니다. 이는 로그 기록, 사용자 행동 추적, 혹은 콘텐츠의 타임스탬프를 찍기 위해 프로그래머들이 자주 사용합니다.

## How to: (어떻게 하나요?)
```PHP
<?php
// 현재 날짜와 시간을 'Y-m-d H:i:s' 포맷으로 얻기
echo date('Y-m-d H:i:s');
// 출력 예: 2023-04-05 14:23:52

// 오늘 날짜만 얻기
echo date('Y-m-d');
// 출력 예: 2023-04-05
?>
```

## Deep Dive (깊이 탐구)
PHP에서 날짜와 시간을 다루는 함수는 과거부터 지금까지 계속 개발되어 왔습니다. `date()` 함수 외에도 `DateTime` 클래스가 있어 객체지향적 접근을 제공합니다. 서버의 시간대 설정이나 사용자의 시간대에 따라 날짜를 조정할 수도 있습니다. 예를 들어, `date_default_timezone_set()` 함수를 이용해서 PHP 스크립트에서 사용되는 기본 시간대를 설정할 수 있죠.

제한적이지만 `time()` 함수를 사용해 유닉스 타임스탬프를 얻고, 이를 `date()` 함수와 결합해서 날짜를 표현할 수도 있습니다.

파일을 수정하거나 데이터베이스에 저장하는 등 시간 감각이 중요한 작업을 할 때, 날짜와 시간은 굉장히 중요합니다. 예를 들어, 당신의 웹 애플리케이션은 최신 포스트를 순서대로 보여주거나, 회원들에게 얼마나 오래전에 회원이 되었는지를 알려주는 기능을 가질 수 있습니다.

## See Also (더 보기)
- [PHP 공식 날짜와 시간 문서](https://www.php.net/manual/en/book.datetime.php)
- [PHP date() 함수](https://www.php.net/manual/en/function.date.php)
- [DateTime 클래스](https://www.php.net/manual/en/class.datetime.php)
- [date_default_timezone_set() 함수](https://www.php.net/manual/en/function.date-default-timezone-set.php)