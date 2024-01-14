---
title:    "PHP: 미래 또는 과거의 날짜 계산하기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜
날짜를 미래나 과거로 계산하는 것에 관심이 있을 수 있으며 이에 참여하는 이유는 다양할 수 있습니다. 예를 들어, 앞으로 언제 어떤 이벤트가 발생할지 알고 싶을 수 있습니다. 또는 아마도 휴가를 계획하고자 할 수도 있습니다.

## 어떻게
날짜를 미래나 과거로 계산하는 것은 PHP에서 매우 쉽게 할 수 있습니다. 우선 다음과 같은 방법으로 현재 날짜와 시간을 가져옵니다.

```PHP
$now = date("Y-m-d H:i:s");
echo $now;
```

이렇게 하면 현재 날짜와 시간이 표시됩니다. 그런 다음 다음과 같은 방법으로 날짜를 미래나 과거로 계산할 수 있습니다.

```PHP
$future_date = date("Y-m-d H:i:s", strtotime("+1 week"));
$past_date = date("Y-m-d H:i:s", strtotime("-1 month"));
echo $future_date; // 1주일 후
echo $past_date; // 1달 전
```

이제 원하는 간격을 설정해서 날짜를 계산하고 출력할 수 있습니다.

## 깊게 파고들기
날짜를 미래나 과거로 계산하는 것은 timestamp를 사용하여 만들어진 UNIX 시스템에서 거의 모든 프로그래밍 언어에서 가능합니다. PHP에서는 `strtotime()` 함수를 사용하여 미래나 과거 날짜를 계산할 수 있습니다. 이 함수는 요일 이름이나 날짜 형식을 매개변수로 받을 수 있으며 다양한 날짜 계산 방법을 제공합니다. 더 자세한 정보를 원하신다면 PHP 공식 문서를 참조해보세요.

## 또 다른 정보 보기
- [PHP 공식 문서](https://www.php.net/manual/en/function.strtotime.php)
- [날짜와 시간을 계산하는데 유용한 함수들](https://www.geeksforgeeks.org/php-date-strtotime-function/)
- [PHP에서 날짜를 다루는 방법](https://www.guru99.com/date-time-and-calendar-in-php.html)

## 참고
본 글은 "날짜를 미래나 과거로 계산하는 방법"에 대해 간략하게 설명한 것입니다. 실제로 프로그래밍할 때에는 다양한 변수나 상황에 따라 다르게 적용될 수 있으니 참고용으로만 이용하시기 바랍니다. 감사합니다.