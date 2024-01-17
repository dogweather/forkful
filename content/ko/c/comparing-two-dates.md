---
title:                "두 날짜 비교하기"
html_title:           "C: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

두 개의 날짜를 비교한다는 것은 무엇을 의미할까요? 프로그래머들이 왜 이를 하는 걸까요? 두 날짜를 비교하는 것은 간단하게 말하면 두 개의 날짜가 어떤 상관관계가 있는지를 알아보는 것입니다. 프로그래머들은 이를 통해 데이터를 정렬하고, 검색하고, 필터링하고, 다양한 연산을 수행할 수 있습니다.

## 방법:

다음은 두 개의 날짜를 비교하는 기본적인 방법을 보여줍니다. ```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = { .tm_year = 2020, .tm_mon = 6, .tm_mday = 1 };
    struct tm date2 = { .tm_year = 2021, .tm_mon = 1, .tm_mday = 1 };

    if (difftime(mktime(&date1), mktime(&date2)) > 0) {
        printf("Date 1 is later than Date 2.");
    } else if (difftime(mktime(&date1), mktime(&date2)) < 0) {
        printf("Date 1 is earlier than Date 2.");
    } else {
        printf("Date 1 and Date 2 are the same.");
    }

    return 0;
}
```

위의 예제 코드는 C의 내장 함수인 ```difftime()```과 ```mktime()```을 사용하여 두 개의 날짜를 비교합니다. 이 함수들은 각각 두 시간 간의 차이를 계산하고, 날짜를 시간 형태로 변환하는 역할을 합니다.

## 깊이있게 살펴보기:

두 날짜를 비교하는 방법은 시간이 지나면서 여러 가지가 있었지만, 현재는 C에서 제공하는 내장 함수를 이용하는 것이 가장 효율적입니다. 다른 프로그래밍 언어에서도 비슷한 함수를 제공하며, 필요에 따라 날짜와 시간을 바로 비교할 수도 있습니다. 또한 비교에 사용되는 시간의 정확성과 해상도를 고려해야 할 수 있습니다.

## 더 알아보기:

- [C의 날짜 및 시간 관련 함수](https://www.tutorialspoint.com/cprogramming/c_date_time.htm)
- [C++의 날짜 및 시간 관련 함수](https://www.cplusplus.com/reference/ctime/)
- [다양한 프로그래밍 언어에서의 날짜 및 시간 비교 방법](https://www.journaldev.com/17184/how-to-compare-two-dates-in-java)