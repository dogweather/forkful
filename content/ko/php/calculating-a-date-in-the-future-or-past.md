---
title:                "PHP: 미래나 과거의 날짜 계산하기"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것이 왜 중요한지 이유에 대해 알아보겠습니다. 이 기능은 실제 세상에서 매우 유용합니다. 예를 들어, 어떤 이벤트가 두 달 후에 발생한다면, 그 날짜를 미리 계산하여 준비를 할 수 있습니다. 또는 과거의 날짜를 계산하여 기념일을 기념하는 데 사용할 수도 있습니다.

## 방법

일정한 가격을 지불하고 특정 상품을 배송받는 날짜를 예측하고 싶습니다. 일반적으로 이런 상황에서는 오늘의 날짜에 일정한 시간(예: 3일, 1주일 등)을 더하는 방법을 사용합니다. 하지만 PHP를 사용하면 더 복잡한 계산도 가능합니다. 아래는 PHP를 사용하여 미래나 과거의 날짜를 계산하는 예제 코드입니다.

```PHP
<?php 

// 오늘 날짜
$today = date("Y-m-d");

// 3일 후의 날짜 계산
$future_date = date("Y-m-d", strtotime("+3 days"));

// 1주일 후의 날짜 계산
$next_week = date("Y-m-d", strtotime("+1 week"));

// 2주 후의 날짜 계산
$two_weeks = date("Y-m-d", strtotime("+2 weeks"));

// 2년 전의 날짜 계산
$past_date = date("Y-m-d", strtotime("-2 years"));

// 결과 출력
echo "오늘 날짜: " . $today . "<br>";
echo "3일 후: " . $future_date . "<br>";
echo "1주일 후: " . $next_week . "<br>";
echo "2주 후: " . $two_weeks . "<br>";
echo "2년 전: " . $past_date . "<br>";

?>
```

위의 코드를 실행하면 다음과 같은 결과를 얻을 수 있습니다.

```
오늘 날짜: 2021-09-01
3일 후: 2021-09-04
1주일 후: 2021-09-08
2주 후: 2021-09-15
2년 전: 2019-09-01
```

## 딥 다이브

PHP에서는 미래나 과거의 날짜를 계산하는 데에 유용한 여러 가지 함수가 있습니다. 그 중에서도 `strtotime()` 함수를 가장 많이 사용합니다. 이 함수는 문자열 형태로 된 날짜나 시간을 타임스탬프로 변환해주는 역할을 합니다. 타임스탬프는 1970년 1월 1일 이후 경과한 초 단위의 값을 나타냅니다.

PHP에서는 또한 `date()` 함수를 사용하여 타임스탬프를 날짜 형식으로 변환할 수 있습니다. `date()` 함수에는 날짜 형식 포맷 문자열을 전달하여 날짜를 원하는 형식으로 표시할 수 있습니다.

## 참고 자료

- [PHP strtotime 함수 설명 (영문)](https://www.php.net/manual/en/function.strtotime.php)
- [PHP date 함수 설명 (영문)](https://www.php.net/manual/en/function.date.php)