---
title:                "두 날짜 비교하기"
html_title:           "PHP: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜 비교하는가?
두 날짜를 비교하는 이유는 프로그래밍에서 매우 일반적인 작업 중 하나이며, 두 날짜가 같은지 또는 한 날짜가 다른 날짜보다 이전인지 늦은지를 판단해야 할 때 발생합니다.

## 방법
우선, 비교하고자 하는 두 날짜를 `DateTime` 객체로 만듭니다. 그런 다음 `DateTime` 객체의 `diff()` 메소드를 사용하여 두 날짜 사이의 차이를 계산합니다.

```PHP
$date1 = new DateTime('2021-01-01');
$date2 = new DateTime('2021-01-02');

$diff = $date1->diff($date2);

echo $diff->days; // 1
```

위의 예시에서는 `diff()` 메소드를 사용하여 두 날짜의 차이를 일 단위로 계산하고 있습니다. 이 외에도 `diff()` 메소드를 사용하여 시나 분 등 다양한 단위로 차이를 계산할 수 있습니다.

## 깊이 파고들기
`diff()` 메소드는 두 개의 `DateTime` 객체의 차이를 계산하는 데에 더 많은 옵션을 제공합니다. 예를 들어, 두 날짜 사이의 차이를 정확한 일수가 아닌, 보다 유연한 방식으로 계산할 수 있습니다.

```PHP
$date1 = new DateTime('2021-01-01');
$date2 = new DateTime('2021-01-02');

$diff = $date1->diff($date2, true); // 두 날짜 사이의 실제 차이를 초 단위로 계산

echo $diff->days; // 1
echo $diff->h; // 23 (23시간 차이)
```

이 외에도 `diff()` 메소드를 사용하여 두 날짜의 차이를 이전 또는 이후의 특정 시간 단위로 제한할 수 있으며, 포맷을 지정하여 출력하는 등 다양한 방식으로 활용할 수 있습니다.

## See Also
- [PHP 공식 문서 - DateTime 클래스](https://www.php.net/manual/en/class.datetime.php)
- [PHP 공식 문서 - DateTime::diff() 메소드](https://www.php.net/manual/en/datetime.diff.php)