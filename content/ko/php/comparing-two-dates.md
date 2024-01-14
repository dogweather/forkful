---
title:                "PHP: 두 날짜 비교하기"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
두 개의 날짜를 비교하는 것에 대해 관심을 가질 수 있는 이유는 여러 가지가 있습니다. 예를 들어, 날짜를 정확하게 비교하면 특정 기간 동안 무엇이 발생했는지 더 잘 이해할 수 있습니다. 또한 날짜를 비교하여 얼마나 많은 시간이 지났는지를 파악할 수 있습니다. 이를 통해 효율적인 일정 계획이나 시간 관리를 할 수 있습니다.

## 어떻게
PHP를 사용하여 두 날짜를 비교하는 방법은 간단합니다. 우선, 비교하고자 하는 두 개의 날짜를 각각 변수에 할당합니다. 그리고 이 변수들을 `strtotime()` 함수를 사용하여 timestamp 형식으로 변환해줍니다. 그리고 `time()` 함수를 사용하여 현재 시간을 timestamp 형식으로 변환한 후, 두 개의 timestamp 값을 비교합니다. 비교한 결과에 따라 원하는 출력을 할 수 있습니다.

```PHP
$first_date = "2020-01-01";
$second_date = "2020-02-01";

$timestamp1 = strtotime($first_date);
$timestamp2 = strtotime($second_date);

$current_time = time();

if ($timestamp1 < $timestamp2) {
    echo "첫 번째 날짜가 두 번째 날짜보다 이전입니다.";
} else if ($timestamp1 > $timestamp2) {
    echo "첫 번째 날짜가 두 번째 날짜보다 미래입니다.";
} else {
    echo "두 날짜가 같습니다.";
}
```

위 코드를 실행하면 다음과 같은 결과를 얻을 수 있습니다.

```
두 번째 날짜가 첫 번째 날짜보다 미래입니다.
```

## 딥 다이브
두 개의 날짜를 비교하는 방법은 간단하지만, 조금 더 깊게 살펴보면 더 많은 것을 배울 수 있습니다. PHP에서 두 날짜를 timestamp로 변환하면, 이는 초 단위의 숫자로 저장됩니다. 따라서 timestamp를 사용하여 시간 간격을 계산하거나, 특정 날짜에 대한 정보를 추출할 수 있습니다. 또한 `date()` 함수를 사용하여 timestamp 값을 날짜 형식으로 변환하여 출력할 수도 있습니다.

## 참고
- [PHP date() 함수](https://www.php.net/manual/en/function.date.php)
- [PHP time() 함수](https://www.php.net/manual/en/function.time.php)
- [PHP strtotime() 함수](https://www.php.net/manual/en/function.strtotime.php)

---

## 참고 자료
- [PHP로 날짜 비교하기](https://zinee-world.tistory.com/245)
- [PHP로 시간 계산하기](https://zetawiki.com/wiki/PHP_%EC%8B%9C%EA%B0%84_%EA%B3%84%EC%82%B0%ED%95%98%EA%B8%B0)