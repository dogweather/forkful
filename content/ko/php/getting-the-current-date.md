---
title:                "PHP: 현재 날짜 가져오기."
simple_title:         "현재 날짜 가져오기."
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜 날짜를 가져오는 것이 중요한가?

날짜는 프로그래밍에서 매우 중요한 정보입니다. 우리가 살고 있는 세상은 빠르게 변하고 있기 때문에, 우리는 항상 최신 날짜에 대한 정보를 가지고 있어야 합니다. 과거의 날짜는 더이상 유효하지 않은 경우가 많고, 최신 날짜를 사용하지 않으면 잘못된 결과를 가져올 수 있습니다. 때문에, 프로그래밍을 할 때에는 항상 최신 날짜를 가져오는 것이 중요합니다.

# 어떻게 현재 날짜를 가져오는가?

PHP에서 현재 날짜를 가져오는 방법은 간단합니다. `date` 함수를 사용하여 현재 날짜와 시간을 출력할 수 있습니다. 예를 들어, 아래와 같은 코드를 작성하면 현재 날짜와 시간이 출력됩니다.

```PHP
echo date("Y-m-d H:i:s");
```

Output:

```2021-07-20 14:00:00```

또한, `strtotime` 함수를 사용하여 다양한 날짜 형식을 변환할 수도 있습니다. 예를 들어, `strtotime("next Monday")`을 사용하면 다음 주 월요일의 날짜를, `strtotime("+2 weeks")`을 사용하면 오늘로부터 2주 뒤의 날짜를 가져올 수 있습니다.

# 깊게 들어가기

현재 날짜를 가져오는 것은 실제로는 더 복잡합니다. 우리가 알고 있는 그레고리력과 달력 시스템의 차이가 있기 때문입니다. 또한, 다양한 시간대와 UTC(Universal Time Coordinated)를 고려해야 할 수도 있습니다. 이러한 복잡한 문제들을 해결하기 위해서는 더 많은 공부가 필요합니다.

# 더 알아보기

- [PHP date 함수 문서](https://www.php.net/manual/en/function.date.php)
- [PHP strtotime 함수 문서](https://www.php.net/manual/en/function.strtotime.php)
- [공용 협정시(UTC)에 대한 정보](https://en.wikipedia.org/wiki/Coordinated_Universal_Time)