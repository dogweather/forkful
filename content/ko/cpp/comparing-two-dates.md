---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 하는가?

두 날짜를 비교하는 것은 특정 시점이 다른 특정 시점보다 앞서거나 뒤에 있는지 판단하는 작업입니다. 이는 일정 관리, 예약 시스템, 파일의 타임스탬프 비교 등에 프로그래머들이 활용합니다.

## 어떻게 하는가:

일반적인 C++을 이용해 두 날짜를 비교하는 방법은 다음 처럼 합니다:

```C++
#include <ctime>

int main() {
    struct tm a = {0,0,0,30,7,2021-1900}; 
    struct tm b = {0,0,0,15,7,2021-1900};

    time_t x = mktime(&a);
    time_t y = mktime(&b);

    double difference = difftime(y, x) / (60 * 60 * 24);
    cout << "Difference is: " << difference << " days\n";
    return 0;
}
```

이 코드의 출력은 다음과 같습니다:

```bash
Difference is: -15 days
```

## 깊은 이해:

날짜의 비교는 컴퓨터 프로그래밍의 오래된 문제입니다. 초기에는 간단히 두 날짜를 숫자로 바꾸어 비교하는 방식이 일반적이었습니다. 하지만 비교를 위해서는 날짜 포맷에 따른 변환을 감당해야 했으므로 복잡성이 일정 수준 유지되었습니다.

표준 C++ 라이브러리는 `struct tm`, `mktime`, `difftime`을 통해 날짜 비교를 보다 쉽게하기 위한 도구를 제공합니다. C++ 20 버전에서는 새로운 날짜와 시간 라이브러리가 추가되어 애플리케이션 개발자들에게 더 유연하고 강력한 도구를 제공하게 되었습니다.

번호를 부여하는 대신 시간을 나타내는 방식에는 Unix 타임스탬프, Julian Date, TAI 등 다양한 대안이 있습니다. 각 방식은 특정 상황에서 장단점을 가집니다.

## 참고 자료:

- [cppreference의 C++ 날짜/시간 라이브러리](http://en.cppreference.com/w/cpp/chrono)
- [StackOverflow 날짜 비교에 대한 토론](https://stackoverflow.com/questions/14218892/compare-two-dates-in-c)
- [C++20의 새로운 날짜/시간 기능에 대해](https://www.modernescpp.com/index.php/the-first-parts-of-date-time-in-c-20)