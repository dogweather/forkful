---
title:                "PHP: 현재 날짜 가져오기"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜: 현재 날짜를 구하는 데 참여해야 할 이유

현재 날짜를 구하는 것은 프로그래머들에게 매우 중요합니다. 이는 소프트웨어에서 날짜를 기반으로 한 여러 가지 기능이 필요하고, 특히 온라인 서비스에서는 이를 사용하는 것이 필수적입니다. PHP는 날짜와 시간을 다루는 데 매우 편리한 기능을 제공합니다.

## 어떻게: 코드 예제와 출력 값 내에서 "```PHP ... ```" 코드 블록을 사용하는 코딩 방법

현재 날짜를 구하는 가장 간단한 방법은 date() 함수를 사용하는 것입니다. 이 함수는 첫 번째 매개변수로 포맷을 지정할 수 있으며, 두 번째 매개변수로는 원하는 날짜와 시간을 지정할 수 있습니다.

```PHP
// 현재 시간과 날짜를 YYYY-MM-DD 형식으로 출력하는 예제
echo date("Y-m-d"); // 2021-03-15
```

출력 값은 현재 날짜를 기준으로 하여 변경될 수 있습니다. 예를 들어, 오늘이 2021년 3월 15일이라면 출력 값은 위의 예제와 같을 것입니다.

또 다른 방법으로는 strtotime() 함수를 사용하는 것입니다. 이 함수는 문자열로 표현된 날짜와 시간을 파싱하여 타임스탬프를 반환합니다. 이를 date() 함수와 함께 사용하면 특정 날짜와 시간에 대한 포맷을 만들 수 있습니다.

```PHP
// 한 달 후의 날짜를 YYYY년 m월 d일 형식으로 출력하는 예제
$futureDate = strtotime("+1 month");
echo date("Y년 m월 d일", $futureDate); // 2021년 04월 15일
```

## 깊이 파고들기: 현재 날짜를 구하는 더 깊은 정보

PHP는 기본적으로 시스템에서 설정된 타임존을 사용하여 날짜와 시간을 반환합니다. 하지만 만약 어느 시간대에서 실행되는 코드를 사용자가 설정한 시간대로 반환하고 싶다면, date_default_timezone_set() 함수를 사용하여 타임존을 변경할 수 있습니다.

또한 날짜와 시간을 반환하는 다양한 함수 외에도, PHP는 날짜와 시간을 계산하는 데에도 유용한 함수들이 있습니다. 예를 들어, date_diff() 함수를 사용하여 두 날짜 간의 차이를 계산할 수 있으며, date_add() 함수를 사용하여 날짜에 일정한 시간 간격을 추가할 수 있습니다.

## 또 다른 참고 자료

- [PHP date() 함수 공식 문서](https://www.php.net/manual/en/function.date.php)
- [PHP strtotime() 함수 공식 문서](https://www.php.net/manual/en/function.strtotime.php)
- [PHP date_default_timezone_set() 함수 공식 문서](https://www.php.net/manual/en/function.date-default-timezone-set.php)
- [PHP date_diff() 함수 공식 문서](https://www.php.net/manual/en/function.date-diff.php)
- [PHP date_add() 함수 공식 문서](https://www.php.net/manual/en/function.date-add.php)