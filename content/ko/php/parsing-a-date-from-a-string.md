---
title:                "문자열에서 날짜 파싱하기"
html_title:           "PHP: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
날짜를 문자열에서 파싱한다는 것은 컴퓨터 프로그래머들이 문자열에서 날짜를 이해하고 처리할 수 있도록 하는 과정입니다. 이는 시간, 날짜 및 다양한 양식의 데이터를 정확하게 분석하기 위해 필요합니다.

## 어떻게:
PHP를 사용하여 문자열에서 날짜를 파싱하는 방법에 대해 알아보겠습니다. 아래의 코드 블록은 문자열에서 날짜를 추출하기 위한 예제입니다.

```PHP
  $dateString = "2020-12-01";
  $date = date_parse($dateString);

  echo $date['month']; // Output: 12
  echo $date['day']; // Output: 01
  echo $date['year']; // Output: 2020
```

## 깊이 파고들기:
(1) 과거에는 문자열에서 날짜를 파싱하는 것이 매우 어려웠습니다. 그러나 지금은 PHP와 같은 언어가 내장 함수를 제공하여 더 쉽게 처리할 수 있게 되었습니다. (2) 날짜 형식에 따라 다르게 파싱할 수도 있으며, 이를 위해 더 복잡한 처리 과정이 필요할 수 있습니다. (3) PHP의 ```date_parse()``` 함수 외에도 ```DateTime``` 클래스를 사용하여 더 정확한 날짜 파싱이 가능합니다.

## 또한 참고:
- PHP 공식 문서 - 문자열에서 날짜 파싱하기: https://www.php.net/manual/en/function.date-parse.php
- PHP 공식 문서 - DateTime 클래스: https://www.php.net/manual/en/class.datetime.php