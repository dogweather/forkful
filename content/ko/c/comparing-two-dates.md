---
title:                "두 날짜 비교하기"
date:                  2024-02-03T17:54:06.334743-07:00
model:                 gpt-4-0125-preview
simple_title:         "두 날짜 비교하기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/comparing-two-dates.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇과 왜?

C에서 두 날짜를 비교한다는 것은 그들 사이의 시간적 관계를 결정하는 것을 의미합니다 - 즉, 한 날짜가 다른 날짜보다 앞선다거나 또는 두 날짜가 동일한지를 판단합니다. 이 기능은 스케줄링, 마감일 또는 기록 유지와 같은 작업을 다루는 응용 프로그램에 있어 중요한데, 시간에 민감한 데이터의 조직화 및 조작을 가능하게 하기 때문입니다.

## 방법:

C에는 날짜를 위한 내장 타입이 없어 `time.h` 라이브러리를 사용하여 날짜와 시간 구조체로 작업할 필요가 있습니다. `tm` 구조체와 `difftime()` 함수는 날짜를 비교하는데 일반적으로 사용됩니다. 아래 예제는 두 날짜를 비교하는 방법을 보여줍니다:

```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = {0};
    struct tm date2 = {0};
    double seconds;

    // 첫 번째 날짜 (YYYY, MM, DD)
    date1.tm_year = 2023 - 1900; // 1900년 이후의 년도
    date1.tm_mon = 3 - 1;        // 월 [0-11]
    date1.tm_mday = 15;          // 월의 일 [1-31]

    // 두 번째 날짜 (YYYY, MM, DD)
    date2.tm_year = 2023 - 1900;
    date2.tm_mon = 4 - 1;
    date2.tm_mday = 14;

    // time_t 형식으로 변환
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // 비교
    seconds = difftime(time1, time2);

    if (seconds == 0) {
        printf("날짜가 동일합니다.\n");
    } else if (seconds > 0) {
        printf("첫 번째 날짜가 두 번째 날짜 이후입니다.\n");
    } else {
        printf("첫 번째 날짜가 두 번째 날짜 이전입니다.\n");
    }

    return 0;
}
```

출력 결과는 다음과 같을 수 있습니다:

```text
첫 번째 날짜가 두 번째 날짜 이전입니다.
```

이 프로그램은 특정 날짜로 두 개의 `tm` 구조체를 초기화하고, `mktime()`을 사용하여 `time_t` 형식으로 변환한 후, 마지막으로 `difftime()`을 사용하여 두 시간 사이의 차이(초 단위로 `double` 형태)를 반환하여 비교합니다.

## 심층 분석

C의 초기 단계에서 날짜와 시간 연산은 윤년, 월의 다양한 일 수, 심지어 윤초 등을 고려한 수동 계산을 요구했습니다. ANSI C 표준에서 `time.h`의 도입은 C에서 시간 처리를 표준화하였고, 날짜 및 시간 연산을 단순화하였습니다.

`time.h`를 사용한 날짜 비교는 간단하지만 제한이 있습니다. `tm` 구조체는 시간대나 일광 절약 시간을 고려하지 않으며, `difftime()`은 초 단위의 차이만 제공하여 특정 응용 프로그램에는 더 세밀한 정밀도가 부족합니다.

시간대 지원, 일광 절약 시간 전환, 더 정확한 시간 간격을 포함한 보다 강력한 날짜-시간 연산이 필요한 응용 프로그램의 경우, `date.h`(표준 라이브러리의 일부가 아닌, 하워드 히난트의 날짜 라이브러리)와 같은 라이브러리는 `time.h`에 대한 현대적인 대안을 제공합니다. 이러한 라이브러리는 C++에서 날짜-시간 조작을 위한 더 포괄적인 도구를 제공하여, 수십 년간의 프로그래밍 언어 설계의 발전에서 이점을 얻습니다. C 프로그래머들은 이러한 외부 라이브러리를 활용하거나 날짜-시간 계산의 복잡성을 직접 정밀하게 처리하여 정확하고 문화적으로 인식된 날짜-시간 조작을 달성하기 위해 필요합니다.
