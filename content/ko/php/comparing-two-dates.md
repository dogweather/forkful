---
title:                "PHP: 두 날짜를 비교하는 방법"
simple_title:         "두 날짜를 비교하는 방법"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜

두 날짜를 비교하는 것에 대해 이야기해보겠습니다. 여러분들은 아마도 어디에서나 날짜 비교를 할 수 있는 다양한 방법들을 본 적이 있을 것입니다. 하지만 PHP를 사용하는 경우에는 다른 언어들과는 다른 방식으로 날짜를 비교해야 합니다. 이 글에서는 PHP를 사용하여 두 날짜를 비교하는 방법에 대해 알아보겠습니다.

# 어떻게

가장 간단한 방법은 ```strtotime()``` 함수를 사용하는 것입니다. 이 함수는 문자열로 표현된 날짜를 타임스탬프로 변환해줍니다. 그리고 ```date()``` 함수를 사용하여 날짜를 원하는 형식으로 출력할 수 있습니다.

```PHP
$date1 = "2021-01-01";
$date2 = "2021-01-02";

$timestamp1 = strtotime($date1);
$timestamp2 = strtotime($date2);

echo "두 날짜의 차이는 " . ($timestamp2 - $timestamp1) . "초 입니다."; // 결과: 두 날짜의 차이는 86400초 입니다.
```

때때로 우리는 두 날짜를 정확한 단위로 비교해야 할 필요가 있습니다. 이 경우, ```date_diff()``` 함수를 사용할 수 있습니다. 이 함수는 두 날짜 사이의 차이를 나타내는 객체를 반환해줍니다.

```PHP
$date1 = "2021-01-01";
$date2 = "2021-01-31";

$diff = date_diff(date_create($date1), date_create($date2));

echo "두 날짜 사이의 차이는 " . $diff->format('주간 %a일') . "입니다."; // 결과: 두 날짜 사이의 차이는 주간 30일입니다.
```

# 깊이 파헤치기

PHP에서 날짜를 비교할 때, 우리는 다양한 형식의 날짜 문자열을 다루어야 합니다. 이는 문자열의 형식이 맞지 않으면 예상치 못한 결과를 낳을 수 있기 때문입니다. 예를 들어, 다음과 같이 문자열로 표현된 날짜를 비교한다고 가정해봅시다.

```PHP
$date1 = "2021-01-31";
$date2 = "31-01-2021";

$timestamp1 = strtotime($date1);
$timestamp2 = strtotime($date2);

echo "두 날짜의 차이는 " . ($timestamp2 - $timestamp1) . "초 입니다."; // 결과: 두 날짜의 차이는 86400초 입니다.
```

두 날짜는 다른 형식으로 표현되지만, PHP는 아무런 에러 없이 결과를 반환합니다. 이는 strtotime() 함수가 문자열을 파싱할 때, 먼저 설정된 시스템로케일을 기준으로 날짜 형식을 파악하고, 그에 맞게 해석하기 때문입니다. 우리는 이러한 예상치 못한 결과를 피하기 위해 항상 날짜 문자열을 ```YYYY-MM-DD``` 형식으로 표기하는 것이 좋습니다.

# 또 다른 참고자료

- https://www.php.net/manual/en/function.strtotime.php
- https://www.php.net/manual/en/function.date.php
- https://www.php.net/manual/en/function.date-diff.php