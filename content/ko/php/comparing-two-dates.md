---
title:                "두 날짜를 비교하는 것"
html_title:           "PHP: 두 날짜를 비교하는 것"
simple_title:         "두 날짜를 비교하는 것"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 비교는 무엇이고 왜 디테일하게 비교해야 할까?

비교는 두 개의 값을 비교하여 더 큰지 또는 작은지를 나타내는 것을 말합니다. 프로그래머들은 비교를 통해 어떤 값이 더 큰지 알 수 있고, 조건문과 같은 로직에서 필요한 결정을 내릴 수 있게 됩니다.

## 어떻게 해야 할까?

```PHP
$date1 = "2021-01-01";
$date2 = "2021-02-01";

// 날짜를 비교하여 더 큰지 작은지를 알 수 있습니다.
if ($date1 > $date2) {
    echo "날짜 1이 더 클까요?";
} else {
    echo "날짜 2가 더 큰걸요?";
}
// 출력: 날짜 2가 더 큰걸요?
```

## 더 깊게 들어가보자

비교는 프로그래밍에서 매우 중요한 개념입니다. 예전에는 비교를 첫 번째 값이 더 큰지 나오기 전에 모든 비트를 비교하는 방식이 일반적이었습니다. 하지만 현재에는 비트 단위 연산으로 비교를 할 수 있게 되면서 더 효율적인 방식으로 개발할 수 있게 되었습니다.

비교를 할 때 알아야 할 다른 방법으로는 strict comparison과 loose comparison이 있습니다. strict comparison은 값과 데이터형까지 동일한지를 비교하는 반면, loose comparison은 값만 비교합니다.

## 관련 자료

- PHP 비교 연산자 문서: https://www.php.net/manual/kr/language.operators.comparison.php
- PHP 비트 단위 연산자 문서: https://www.php.net/manual/kr/language.operators.bitwise.php