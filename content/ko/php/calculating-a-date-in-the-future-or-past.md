---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "PHP: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜 

오늘의 날짜를 기준으로 미래나 과거의 다른 날짜를 계산하는 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 이용자가 생일 추카 메시지를 받거나 할 일 목록에 특정 날짜를 추가하는 등의 상황에서 필요할 수 있습니다.

## 어떻게 하나요 

만약 오늘이 2020년 11월 16일이라고 가정해봅시다. 다음과 같은 PHP 코드를 사용하여 `2020년 11월 23일`을 계산해볼 수 있습니다. 

```PHP
$date = date("Y-m-d", strtotime("next monday"));
echo $date; // 2020-11-23
```

위 코드에서 `strtotime()` 함수는 미래의 날짜를 계산하기 위해 `+` 연산자를 사용합니다. 여러분은 이 연산자를 조합하여 원하는 날짜를 계산할 수 있습니다. 예를 들어, `next monday + 1 week`을 입력하면 다음주 월요일이 아닌 다다음주 월요일이 출력될 것입니다. 

또한 `strtotime()` 함수는 과거의 날짜를 계산할 때 `-` 연산자를 사용합니다. 예를 들어, `-1 day`를 입력하면 어제의 날짜가 출력됩니다.

## 깊이 파고들기 

`strtotime()` 함수는 크게 두 가지 타입으로 나눌 수 있습니다. 첫 번째는 상대적인 날짜 표현 방식입니다. 위에서 언급한 `next`나 `+` 처럼 다음주 월요일이나 한 주 후의 날짜를 계산하는 것이 이에 해당합니다.

두 번째 타입은 고정된 날짜 표현 방식입니다. 예를 들어 `February 14th, 2021`과 같이 정확한 날짜를 입력하면 해당 날짜를 계산할 수 있습니다. 

가끔씩 날짜 계산을 하는데 알고리즘이 복잡할 수 있습니다. 이런 경우에는 [PHP 공식 문서](https://www.php.net/manual/kr/function.strtotime.php)를 참고하는 것이 유용합니다. 

## 참고 자료 

- [PHP 공식 문서 for `strtotime()`](https://www.php.net/manual/kr/function.strtotime.php)
- [W3Schools PHP Date and Time](https://www.w3schools.com/php/php_date.asp)