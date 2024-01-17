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

# 무엇과 왜?

날짜를 미래나 과거로 계산하는 것은 프로그래머들이 일반적으로 하는 작업입니다. 이는 미래나 과거의 특정 날짜를 계산하기 위해 사용될 수 있기 때문입니다. 예를 들어, 이 기능은 패턴을 기반으로 7일 후의 날짜를 계산하거나 지난 주 월요일의 날짜를 찾는 데 사용할 수 있습니다.

# 어떻게:

프로그래밍에서 날짜를 계산하는 것은 전체적으로 간단한 작업이지만, 많은 방법이 있을 수 있습니다. 다음은 PHP에서 날짜를 계산하는 데 사용할 수 있는 두 가지 예제입니다.

### 예제 1:

```PHP
// 7일 후의 날짜 계산
$date = date("Y-m-d", strtotime("+7 days"));
echo $date; // 예시 출력: 2021-09-28
```

### 예제 2:

```PHP
// 지난 주 월요일의 날짜 계산
$lastMonday = date("Y-m-d", strtotime("last Monday"));
echo $lastMonday; // 예시 출력: 2021-09-20
```

# 심층 분석:

### 역사적 맥락:

PHP의 날짜 계산 기능은 프로그래밍 언어 혁신으로 볼 수 있습니다. 이 기능은 이전의 다른 언어에 비해 간편한 문법을 제공합니다. 이것은 프로그래머들이 날짜나 시간 관련 작업을 더 쉽게 할 수 있도록 해주었습니다.

### 대안:

날짜를 계산하는 또 다른 방법으로는 PHP의 DateTime 클래스를 사용하는 것이 있습니다. 이 클래스는 날짜와 시간을 다루는 더 많은 기능을 제공합니다.

### 구현 세부 정보:

PHP에서 날짜를 계산하는 함수의 구현은 내부적으로 복잡하게 되어 있습니다. 그렇기 때문에 문제 발생 시 디버깅이 어려울 수 있습니다. 따라서 날짜 계산을 수행할 때는 다른 함수와의 조합을 최소화하여 가능한 한 단순한 코드를 유지하는 것이 좋은 관행입니다.

# 관련 정보:

1. PHP 공식 문서: [날짜 함수와 타임존](https://www.php.net/manual/en/function.date.php)
2. PHP DateTime 클래스 문서: [PHP DateTime 클래스](https://www.php.net/manual/en/class.datetime.php)
3. 날짜 함수를 사용한 날짜 계산 예제: [날짜 계산 예제](https://www.w3schools.com/php/php_date.asp)