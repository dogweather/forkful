---
title:                "PHP: 미래나 과거의 일자 계산"
simple_title:         "미래나 과거의 일자 계산"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 대해 관심이 생겼다면, 이 블로그 포스트는 여러분을 위한 것입니다! 이 기능은 PHP에서 매우 유용하며, 여러분의 애플리케이션에서 날짜와 시간을 적절하게 다룰 수 있게 해줍니다.

## 방법

우리는 여기서 여러분에게 날짜를 계산하는 방법을 보여드릴 것입니다. 아래의 코드 블록을 사용하여 예제와 출력을 살펴보세요.

```PHP
// 현재 날짜
$today = date("Y-m-d");

// 내일 날짜 계산
$tomorrow = date("Y-m-d", strtotime("+1 day"));

// 어제 날짜 계산
$yesterday = date("Y-m-d", strtotime("-1 day"));

echo "오늘의 날짜: " . $today;
echo "내일의 날짜: " . $tomorrow;
echo "어제의 날짜: " . $yesterday;

// 결과:
// 오늘의 날짜: 2021-11-01
// 내일의 날짜: 2021-11-02
// 어제의 날짜: 2021-10-31
```

위의 예제에서 우리는 `date()` 함수와 `strtotime()` 함수를 사용하여 날짜를 계산했습니다. `date()` 함수는 특정 형식으로 날짜를 반환하며, `strtotime()` 함수는 문자열 표기를 날짜로 변환합니다. 따라서 우리는 `strtotime()` 함수를 이용하여 날짜를 계산한 다음, `date()` 함수를 이용하여 해당 날짜를 우리가 원하는 형식으로 표현할 수 있습니다.

## 깊이 들어가기

보다 정확하고 복잡한 날짜 계산을 위해서는 `DateTime` 클래스를 사용할 수 있습니다. 이 클래스는 날짜와 시간을 다루는 다양한 메서드들을 제공하여 보다 편리하게 날짜를 계산할 수 있게 해줍니다. 아래의 예제를 살펴보세요.

```PHP
// 현재 날짜와 시간을 생성
$now = new DateTime();

// 내일 날짜 계산
$tomorrow = $now->modify("+1 day")->format("Y-m-d");

// 어제 날짜 계산
$yesterday = $now->modify("-1 day")->format("Y-m-d");

echo "오늘의 날짜: " . $now->format("Y-m-d");
echo "내일의 날짜: " . $tomorrow;
echo "어제의 날짜: " . $yesterday;

// 결과:
// 오늘의 날짜: 2021-11-01
// 내일의 날짜: 2021-11-02
// 어제의 날짜: 2021-10-31
```

위의 예제에서는 `DateTime` 클래스를 사용하여 오늘의 날짜와 시간을 생성한 다음, `modify()` 메서드를 이용하여 날짜를 계산했습니다. 마지막으로 `format()` 메서드를 사용하여 날짜를 원하는 형식으로 반환했습니다.

## 참고하기

- PHP 공식 문서: [http://php.net/manual/kr/function.date.php](http://php.net/manual/kr/function.date.php)
- PHP DateTime 클래스 공식 문서: [http://php.net/manual/kr/class.datetime.php](http://php.net/manual/kr/class.datetime.php)
- PHP strtotime() 함수 공식 문서: [http://php.net/manual/kr/function.strtotime.php](http://php.net/manual/kr/function.strtotime.php)

## 참고하기

- PHP 공식 문서: [http://php.net/manual/kr/function.date.php](