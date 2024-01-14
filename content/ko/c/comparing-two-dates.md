---
title:    "C: 두 날짜 비교하기"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜를 비교하는 작업을 왜 하는지 궁금하신가요? 이번 글에서는 날짜를 비교하는 이유와 방법, 그리고 좀 더 깊은 내용을 살펴보겠습니다. C 프로그래밍을 배우고 계신 분들에게 유용한 정보가 될 것입니다.

## 방법

날짜를 비교하는 작업은 C 언어에서 꽤 자주 사용됩니다. 때로는 두 날짜 중 어느 쪽이 더 이후인지를 알아야 할 때가 있거나, 주어진 날짜가 윤년인지를 판단해야 할 때가 있습니다. 이를 위해서는 먼저 `time.h` 헤더 파일을 포함시켜야 합니다.

```
#include <stdio.h>
#include <time.h>

int main() {
  // 비교하고 싶은 날짜를 구조체로 선언합니다.
  struct tm date1 = {.tm_year = 2020, .tm_mon = 8, .tm_mday = 15};
  struct tm date2 = {.tm_year = 2021, .tm_mon = 8, .tm_mday = 15};

  // mktime() 함수를 사용해 날짜를 time_t 형식으로 변환합니다.
  time_t t1 = mktime(&date1);
  time_t t2 = mktime(&date2);

  // difftime() 함수를 사용해 두 날짜 간의 차이를 계산합니다.
  double diff = difftime(t2, t1);

  // 결과를 출력합니다.
  printf("두 날짜의 차이는 %lf일입니다.", diff / 86400);

  // 비교 결과를 반환합니다.
  return 0;
}
```

위 코드를 실행하면 두 날짜 간의 차이가 출력됩니다. 만약 더 이후인 날짜가 뒤에 오도록 하고 싶다면 `t1`과 `t2`를 바꿔주면 됩니다.

```
두 날짜의 차이는 366일입니다.
```

## 깊이 파헤치기

C 언어에서 날짜를 다루는 데에는 여러 가지 방법이 있지만, 위 예제에서는 `time.h` 헤더 파일을 사용했습니다. 이 헤더 파일은 시간과 날짜를 정의하는데 사용되는 여러 가지 구조체와 함수들을 포함하고 있습니다.

`struct tm`은 시간과 날짜 정보를 저장하는 구조체로서 날짜 구성 요소들을 멤버 변수로 가지고 있습니다. 위 예제에서는 날짜 구성 요소들을 `.tm_year`, `.tm_mon`, `.tm_mday`와 같은 멤버 변수에 직접 값을 할당해주었습니다.

`mktime()` 함수는 구조체에 저장된 날짜를 `time_t` 형식으로 변환해주는 함수입니다. 이를 사용해 비교하고 싶은 날짜를 먼저 구조체로 선언한 뒤 `mktime()` 함수를 호출해 `time_t` 형식으로 변환해줍니다.

`difftime()` 함수는 두 개의 `time_t` 값을 받아 그 차이를 계산해주는 함수입니다. 위 예제에서는 이 함수를 사용해 두 날짜 간의 차이를 계산한 뒤 출력해주었습니다.

## 연관 글

- [Date and Time Functions in C](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Working with time and date in C](https://www.geeksforgeeks.org/working-with-time-and-date-in-c/)
- [Date and Time in C](https://www.programiz.com/c-programming/c-date-time)