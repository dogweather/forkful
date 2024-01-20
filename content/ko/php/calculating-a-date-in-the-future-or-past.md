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

## 뭘 & 왜? 

미래나 과거 날짜 계산은 특정 기간 후 또는 전의 날짜를 찾는 프로세스를 의미합니다. 프로그래머들은 이를 통해 스케줄링, 예약, 경과 시간 계산 등 다양한 기능을 구현합니다.

## 구현 방법: 

PHP에서 미래나 과거의 날짜를 계산하는 방법은 매우 간단합니다.

```PHP
 <?php
    // 현재 날짜를 가져옵니다.
    $today = new DateTime();
    echo $today->format('Y-m-d H:i:s');
   
    // 일주일 후의 날짜를 계산합니다.
    $oneWeek = new DateInterval('P7D');
    $futureDate = $today->add($oneWeek);
    echo $futureDate->format('Y-m-d H:i:s');
?>
```

출력 결과는 다음과 같습니다:

```
2022-02-26 08:00:00
2022-03-05 08:00:00
```

## 깊은 탐색:

과거의 PHP 버전에서는 `strtotime()` 함수를 이용했으나 더욱 발전된 `DateTime` 클래스를 사용하면 더욱 강력한 기능을 수행할 수 있습니다. 

```PHP
<?php
  $pastDate = strtotime("-1 week");
  echo date('Y-m-d', $pastDate);
?>
```

그러나 DateTime 클래스를 사용하면 시간대 지원 및 날짜 간격 계산 등의 추가적인 기능을 제공합니다.  

현재 버전에서는 더 간편하고 읽기 쉬운 코드를 작성할 수 있도록 `DateInterval` 클래스를 사용하여 날짜를 처리하는 방법이 제공됩니다.

## 추가 정보:

아래는 PHP 날짜 계산에 관한 추가 리소스 링크입니다:

1. [PHP Manual: DateTime 클래스](https://www.php.net/manual/en/class.datetime.php): 현재의 PHP 날짜와 시간에 대한 공식 매뉴얼.
2. [PHP Manual: DateInterval 클래스](https://www.php.net/manual/en/class.dateinterval.php): PHP 날짜 간격 계산에 대한 공식 문서입니다.
3. [Stack Overflow: PHP Date Time 관련 질문](https://stackoverflow.com/questions/tagged/php+datetime): 프로그래머 간의 질 답 게시판 입니다.