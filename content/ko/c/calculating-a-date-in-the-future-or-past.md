---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "C: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

미래 혹은 과거의 날짜를 계산하는 것은 특정 기간 후나 전의 날짜를 얻는 프로그래밍 작업입니다. 이는 사용자에게 리마인더를 보내거나, 일정을 계획하는 등 다양한 기능에 사용됩니다.

## 어떻게 하는가:

다음은 C 프로그래밍 언어를 이용한 날짜 계산 방법입니다:

```C
#include <stdio.h>
#include <time.h>

int main() {
   time_t N = 5 * 24 * 60 * 60;  // 5일 후
   time_t future;

   time(&future);  // 현재 시간 획득
   future += N;    // 미래 시간 계산

   printf("예정 날짜: %s", ctime(&future));

   return 0;
}
```

이 코드는 현재 날짜에서 5일 후의 날짜를 출력합니다.

## 심층 이해

1. **역사적 배경**: 초기 컴퓨터 시스템에서는 시스템 시간을 UNIX epoch (1970년 1월 1일)부터의 초 단위로 표현하였습니다. 이 방법은 다양한 시간과 날짜 계산에 효과적이었습니다.
2. **대안**: 이외에도 `localtime()`, `mktime()`, `strftime()`과 같은 함수를 이용하여 더 복잡한 날짜 연산도 가능합니다.
3. **구현 세부 사항**: `time()` 함수는 시스템의 칼렌더 시간을 가져옵니다. `ctime()` 함수는 `time_t` 형식의 시간을 문자열로 변환합니다.

## 참고 자료:

- [C Library - <time.h>](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [C Programming/Time](https://en.wikibooks.org/wiki/C_Programming/Time)
- [UNIX 시간과 날짜 관련 함수](https://www.gribble.org/c/programming2.html)