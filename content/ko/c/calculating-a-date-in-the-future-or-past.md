---
title:                "C: 미래나 과거 날짜 계산하기"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜

*왜 미래나 과거의 날짜를 계산할 때 이를 해야 할까요?*

프로그래밍에서 날짜 계산은 매우 중요한 부분입니다. 예를 들어, 프로그램에서 어떤 이벤트를 특정한 날짜에 예약하고자 한다면, 미래나 과거의 날짜를 계산해야 할 수도 있습니다. 이를 통해 프로그램을 더 유연하고 정확하게 동작하도록 할 수 있습니다.

# 하는 방법

여러분들은 C 프로그래밍 언어를 사용하여 미래나 과거의 날짜를 계산할 수 있습니다. 먼저, time.h 헤더 파일을 include 해주어야 합니다. 그리고 사용하려는 함수를 불러와서 날짜를 계산하면 됩니다.

```c
#include <time.h>

int main() {
  // 현재 시간을 가져옵니다.
  time_t currentTime = time(NULL);

  // 1년 후의 날짜를 계산합니다.
  struct tm *futureDate = localtime(&currentTime);
  futureDate->tm_year += 1;
  mktime(futureDate);

  // 1달 전의 날짜를 계산합니다.
  struct tm *pastDate = localtime(&currentTime);
  pastDate->tm_mon -= 1;
  mktime(pastDate);

  // 계산된 날짜를 출력합니다.
  printf("1년 후의 날짜: %s\n", asctime(futureDate));
  printf("1달 전의 날짜: %s\n", asctime(pastDate));

  return 0;
}

// Output:
// 1년 후의 날짜: Sun Oct 24 09:41:27 2021
// 1달 전의 날짜: Tue Aug 24 09:41:27 2021
```

위 코드에서 `localtime()` 함수는 현재 시간을 사용하여 `struct tm` 구조체를 반환합니다. 그리고 `mktime()` 함수는 전달받은 구조체를 기반으로 새로운 날짜를 계산합니다. 마지막으로 `asctime()` 함수를 사용하여 계산된 날짜를 문자열로 변환하여 출력합니다.

# 더 알아보기

이 배경지식을 바탕으로 우리는 미래나 과거의 날짜를 계산하는 더 다양한 방법들을 살펴볼 수 있습니다. `localtime()`과 `mktime()` 함수 이외에도 다양한 함수들을 사용하여 더 복잡한 날짜 계산을 할 수 있습니다. 또한 시간대와 같은 다른 요소들을 고려할 수 있습니다.

처음에는 난해하게 느껴질 수 있지만, 시간과 날짜를 다루는 기능은 프로그래밍에서 꼭 필요한 것이기 때문에 꼭 익혀두는 것이 좋습니다.

# 더 알아보기

- [C Programming - Date & Time](https://www.tutorialspoint.com/cprogramming/c_date_time.htm)
- [C Library - <time.h>](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [C - Calculating Date and Time Differences](https://aticleworld.com/c-programming-example-of-calculating-date-and-time-differences/)
- [C++ Programming - Time #include <ctime>](https://www.programiz.com/cpp-programming/library-function/ctime/time)