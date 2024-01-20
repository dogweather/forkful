---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

두 날짜를 비교한다는 것은 특정 시점의 두 날짜가 어떻게 서로 관련되어 있는지 결정하기 위함입니다. 프로그래머들은 이를 통해 이벤트 순서, 날짜 차이 계산 등 다양한 작업을 수행합니다.

## 실행 방법:

이제 C 프로그램으로 두 날짜를 비교하는 방법을 알아보겠습니다. `tm` 구조체와 `mktime`, `difftime` 함수를 사용합니다.

```C
#include <time.h>
#include <stdio.h>

int main() {
    struct tm a = { .tm_year=2020-1900, .tm_mon=6, .tm_mday=17};
    struct tm b = { .tm_year=2021-1900, .tm_mon=6, .tm_mday=17};
    
    time_t x = mktime(&a);
    time_t y = mktime(&b);

    double difference = difftime(y, x) / (60 * 60 * 24);
    printf("날짜 차이는 %.f 일입니다\n", difference);
    
    return 0;
}
```

위 예제 출력:

```C
날짜 차이는 365 일입니다
```

## Deep Dive:

두 날짜를 비교하는 방법은 과거로 거슬러 올라가면 여러 방법이 있었습니다. 예를 들어, 두 날짜를 단순히 문자열로 변환하여 비교하는 경우도 있었지만, 이 방법은 날짜들 사이의 차이점을 계산하는 데 한계가 있었습니다. 그래서 최근에는 `tm` 구조체와 `difftime`을 사용하여 보다 정확하게 이 작업을 수행하게 되었습니다.

또한, C언어에서 두 날짜 비교 외에도 `strftime`나 `strptime`과 같은 함수를 사용하여 날짜를 다른 형식으로 변환하는 것도 가능합니다.

## 참고 자료:

아래 링크들은 날짜 비교와 관련된 다양한 주제에 대한 추가 정보를 제공합니다.

1. C library function - difftime(): https://www.tutorialspoint.com/c_standard_library/c_function_difftime.htm
2. C library function - mktime(): https://www.tutorialspoint.com/c_standard_library/c_function_mktime.htm
3. Manipulating date and time in C: https://www.geeksforgeeks.org/date-time-manipulation-c/