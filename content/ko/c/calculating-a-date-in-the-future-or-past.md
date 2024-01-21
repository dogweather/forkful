---
title:                "미래 혹은 과거의 날짜 계산하기"
date:                  2024-01-20T17:28:39.554249-07:00
model:                 gpt-4-1106-preview
html_title:           "Bash: 미래 혹은 과거의 날짜 계산하기"
simple_title:         "미래 혹은 과거의 날짜 계산하기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜 계산하면, 특정한 날짜에서 미래나 과거의 날짜를 찾습니다. 프로그래머는 예약 시스템, 만기일 계산, 기간 추적 같은 기능을 구현할 때 이를 사용합니다.

## How to: (방법)
```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t rawtime;
    struct tm * timeinfo;
    int daysToAdd = 10; // 미래로 10일 더하기

    time(&rawtime); // 현재 시각을 가져옵니다
    timeinfo = localtime(&rawtime); // 현재 지역 시간으로 변환합니다

    printf("오늘 날짜: %s", asctime(timeinfo));
    
    timeinfo->tm_mday += daysToAdd; // 날짜에 10일을 더합니다
    mktime(timeinfo); // 변경된 시간정보를 정규화

    printf("미래의 날짜: %s", asctime(timeinfo));

    return 0;
}
```
Sample Output:
```
오늘 날짜: Fri Apr  7 21:34:55 2023
미래의 날짜: Mon Apr 17 21:34:55 2023
```
`mktime` 함수는 `tm` 구조체의 변화된 값을 기준으로 시간을 재조정하고, 새 날짜를 계산합니다.

## Deep Dive (심층 분석)
과거에는 시간과 날짜 계산을 위한 표준화된 함수가 없었습니다. 각 시스템이나 언어마다 서로 다른 방식을 사용했습니다. C 표준 라이브러리는 `time.h` 헤더 파일에 시간 관련 구조체와 함수를 도입하여 이 문제를 해결했습니다.

대안으로, `time.h` 외에도 많은 시스템에서 `chrono` 라이브러리와 같은 최신 도구를 제공합니다. 하지만, C 언어에서는 `tm` 구조체와 함께 `mktime`, `localtime` 같은 클래식한 함수들이 여전히 중요합니다.

날짜를 계산할 때 주의해야 할 것은 윤년과 시간대, 일광 절약 시간 같은 요소를 고려해야 한다는 점입니다. `mktime` 함수는 이러한 요소들을 자동으로 처리해 줍니다. 하지만, 날짜를 더하기나 빼기만 하는 단순한 계산은 여러분 몫입니다.

## See Also (참고 자료)
- [C Date and Time](https://www.tutorialspoint.com/c_standard_library/c_function_mktime.htm) - `mktime` 함수와 시간 라이브러리에 대한 자세한 정보.
- [C Library - <time.h>](https://www.cplusplus.com/reference/ctime/) - C 라이브러리의 `time.h` 헤더 파일에 대한 설명 및 함수 목록.
- [C Programming/C Reference/time.h](https://en.wikibooks.org/wiki/C_Programming/C_Reference/time.h) - C 프로그래밍에서 시간 처리에 대한 유용한 안내.