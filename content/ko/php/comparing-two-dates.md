---
title:    "PHP: 두 날짜 비교하기"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## 왜 비교하는가

PHP 프로그래밍을 시작하면 가장 많이 사용되는 작업 중 하나는 날짜와 시간을 비교하는 것입니다. 이를 통해 일정한 조건에 따라 프로그램의 흐름을 제어할 수 있기 때문에 날짜를 비교하는 작업은 매우 유용합니다. 이 글에서는 두 날짜를 비교하는 방법에 대해 알아보겠습니다.

## 비교하는 방법

날짜의 비교는 PHP에서 제공하는 `DateTime` 클래스를 사용합니다. 우선, 비교할 날짜를 `DateTime` 클래스로 인스턴스화합니다.

```PHP
$date1 = new DateTime('2021-12-25');
$date2 = new DateTime('2021-12-31');
```

이제 두 날짜를 비교하기 위해 `DateTime` 클래스에서 제공하는 `diff()` 메소드를 사용합니다. 이 메소드는 두 날짜 간의 차이를 반환합니다. 반환 값은 `DateInterval` 클래스의 인스턴스이며, 이를 사용하여 두 날짜 간의 일 수, 월 수, 년 수를 확인할 수 있습니다.

```PHP
$dateInterval = $date1->diff($date2);

echo $dateInterval->days; // 6
echo $dateInterval->months; // 0
echo $dateInterval->years; // 0
```

이처럼 `DateTime` 클래스를 사용하면 간단하게 두 날짜를 비교할 수 있습니다.

## 더 깊게 살펴보기

PHP에는 `DateTime` 클래스와 함께 사용할 수 있는 다양한 메소드와 속성이 있습니다. 예를 들어 `diff()` 메소드 대신 `compare()` 메소드를 사용하면 두 날짜를 비교하여 이전 날짜인지, 이후 날짜인지 아니면 같은 날짜인지를 확인할 수 있습니다.

또한, `DateTime` 클래스는 다양한 날짜 포맷을 지원하므로 사용자에게 편리한 포맷으로 날짜를 표시할 수 있습니다. 이 밖에도 `DateTime` 클래스를 활용하여 날짜와 관련된 다양한 작업을 할 수 있습니다.

## 더 알아보기

이외에도 PHP에서는 날짜와 시간을 다루는 데 유용한 다양한 함수와 클래스를 제공합니다. 더 자세한 내용은 PHP 공식 문서를 참고하시기 바랍니다.

#### See Also

- PHP 공식 문서: https://www.php.net/manual/en/datetime.compare.php
- PHP 날짜 함수 및 클래스: https://www.php.net/manual/en/ref.datetime.php