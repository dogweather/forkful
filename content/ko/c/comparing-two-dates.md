---
title:                "두 날짜 비교하기"
date:                  2024-01-20T17:32:40.485526-07:00
model:                 gpt-4-1106-preview
simple_title:         "두 날짜 비교하기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
두 날짜를 비교하는 것은 두 다른 시점을 기준으로 순서나 간격을 확인하는 프로세스입니다. 프로그래머들은 기한을 확인하거나, 시간 간격을 계산하고, 시간 순서에 따른 이벤트를 정렬하기 위해 날짜를 비교합니다.

## How to:
```c
#include <stdio.h>
#include <time.h>

int compare_dates(struct tm date1, struct tm date2) {
    // Convert struct tm to time_t
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    if (time1 < time2) {
        return -1; // date1 이 이전임
    } else if (time1 > time2) {
        return 1;  // date1 이 이후임
    } else {
        return 0;  // 두 날짜가 동일함
    }
}

int main() {
    struct tm date1 = {.tm_year=123, .tm_mon=4, .tm_mday=5}; // 2023년 5월 5일
    struct tm date2 = {.tm_year=123, .tm_mon=4, .tm_mday=8}; // 2023년 5월 8일
    int result = compare_dates(date1, date2);
    printf("비교 결과: %d\n", result);
    return 0;
}
```

Sample output:
```
비교 결과: -1
```

## Deep Dive
C 프로그래밍에서 날짜를 비교하려면 <time.h> 헤더에 정의된 `time_t` 타입과 `struct tm` 구조체를 이해해야 합니다. 1970년 이후 초로부터의 경과 시간을 나타내는 `time_t` 형식은 날짜를 비교할 때 자주 사용됩니다.

그 이외에도, 문자열로 표현된 날짜를 `struct tm`로 변환하기 위해 `strptime` 함수를 사용할 수 있으며, 날짜 간의 차이를 계산하기 위해서는 `difftime` 함수를 사용할 수 있습니다.

함수 구현 세부사항에선, `mktime` 함수가 `struct tm` 객체를 `time_t` 타입으로 변환하고, 이를 사용해 시간 비교를 수행할 수 있습니다. 이 구현 방법은 시간대 변환과 관련된 이슈를 단순화하고, 표준 시간대 기준으로 일관된 비교를 가능하게 합니다.

## See Also

- C `time.h` reference: https://en.cppreference.com/w/c/chrono
- GNU C Library: Date and Time Functions - https://www.gnu.org/software/libc/manual/html_node/Date-and-Time.html
- Stack Overflow: C date and time functions - https://stackoverflow.com/questions/tagged/c+datetime
