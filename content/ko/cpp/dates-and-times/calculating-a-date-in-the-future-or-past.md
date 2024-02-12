---
title:                "미래나 과거의 날짜 계산하기"
aliases:
- /ko/cpp/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:30:59.584527-07:00
model:                 gpt-4-1106-preview
simple_title:         "미래나 과거의 날짜 계산하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 뭐고 왜? (What & Why?)
미래나 과거의 날짜 계산이란 특정 날로부터 일정 기간을 더하거나 빼서 다른 날짜를 찾는 것입니다. 프로그래머들은 기간 계산, 기념일, 예정된 이벤트 관리 등을 위해 이를 사용합니다.

## 어떻게? (How to:)
```C++
#include <iostream>
#include <chrono>
#include <date/date.h>  // 필요한 경우 https://github.com/HowardHinnant/date에서 date 라이브러리 설치

using namespace std;
using namespace std::chrono;
using namespace date;

int main() {
    // 오늘 날짜를 구함
    auto today = floor<days>(system_clock::now());
    year_month_day ymd = year_month_day{today};
    
    // 미래 날짜 예제: 오늘부터 100일 후
    year_month_day ymd_future = ymd + days{100};
    cout << "100일 후 날짜: " << ymd_future << "\n";
    
    // 과거 날짜 예제: 오늘부터 100일 전
    year_month_day ymd_past = ymd - days{100};
    cout << "100일 전 날짜: " << ymd_past << "\n";
    
    return 0;
}
```
복잡한 설치 없이 실행하고 싶다면, `<chrono>` 라이브러리만 사용해도 대부분의 일반적인 날짜 계산이 가능합니다.

## 깊게 들어가기 (Deep Dive)
날짜 계산은 시간이 지남에 따라 복잡해졌습니다. 과거에는 율리우스력을 사용하다가 그레고리력으로 전환되었습니다. C++20 이전에는 `<ctime>` 라이브러리를 사용하였지만 유연하지 못했습니다. C++20부터는 `<chrono>` 라이브러리가 강화되었고, Howard Hinnant가 만든 `date` 라이브러리를 통해 더 다양하고 정교한 날짜 처리가 가능해졌습니다.

## 참고자료 (See Also)
- [Howard Hinnant's Date library](https://github.com/HowardHinnant/date) - 복잡한 날짜 연산에 적합한 C++ 라이브러리입니다.
- [cppreference.com](https://en.cppreference.com/w/cpp/chrono) - `<chrono>` 라이브러리에 대한 자세한 정보를 제공합니다.
- [C++ Standards Support in GCC](https://gcc.gnu.org/projects/cxx-status.html) - GCC에서 C++ 표준 지원 현황을 파악합니다.
- [Stack Overflow](https://stackoverflow.com/) - 특정 문제에 대한 구체적인 해결책을 질문하고 답변할 수 있는 포럼입니다.
