---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

데이터를 문자열로 변환한다는 것은 날짜나 시간 데이터를 텍스트 형태로 바꾸는 행위를 말합니다. 이러한 행위는 날짜나 시간 정보를 사용자에게 보기 좋게 출력하거나, 문자열 형태로 데이터를 저장하고 처리해야 할 경우에 필요합니다.

## 어떻게 사용하는가:

PHP에서는 `date_format()` 함수를 사용하여 날짜 정보를 문자열로 변환할 수 있습니다. 아래의 예제 코드를 참고하세요.

```PHP
<?php

// Date 객체 생성
$date = new DateTime('2021-05-17');

// 문자열로 변환
echo date_format($date, 'Y-m-d H:i:s');
```

위 코드를 실행하면, 다음과 같은 결과가 출력됩니다.

```
2021-05-17 00:00:00
```

## 깊이 있는 정보:

### 역사적 맥락
PHP에서 날짜와 시간을 다루기 위한 다양한 함수들은 PHP4부터 도입되었으며, 그 중 `date_format()` 함수는 PHP5부터 사용 가능하게 되었습니다.

### 대체 방법
`date_format()` 외에도 `strftime()` 함수를 사용해 날짜 정보를 문자열로 변환할 수 있습니다. 또한, 날짜 정보를 ISO 8601 형식으로 변환하기 위해서는 `DateTime::ISO8601`를 사용하면 됩니다.

### 구현 세부 사항
`date_format()` 함수는 첫 번째 인자로 `DateTime` 객체를, 두 번째 인자로 원하는 형식(format)을 문자열로 받습니다. `DateTime` 객체는 날짜와 시간 관련 정보를 저장하고, 원하는 형식의 문자열로 변환하는 데에 사용됩니다.

## 참고 자료:
다음은 날짜를 문자열로 변환하는 방법과 관련된 참고 자료들입니다.
- 샘플 코드: [PHP Date/Time Functions](https://www.w3schools.com/php/php_ref_date.asp) 
- TutorialsPoint 글: [PHP - Function date_format()](https://www.tutorialspoint.com/php/php_function_date_format.htm)