---
date: 2024-01-20 17:30:59.584527-07:00
description: "\uC5B4\uB5BB\uAC8C? (How to:) \uBCF5\uC7A1\uD55C \uC124\uCE58 \uC5C6\
  \uC774 \uC2E4\uD589\uD558\uACE0 \uC2F6\uB2E4\uBA74, `<chrono>` \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uB9CC \uC0AC\uC6A9\uD574\uB3C4 \uB300\uBD80\uBD84\uC758 \uC77C\uBC18\
  \uC801\uC778 \uB0A0\uC9DC \uACC4\uC0B0\uC774 \uAC00\uB2A5\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.315023-06:00'
model: gpt-4-1106-preview
summary: "(How to:) \uBCF5\uC7A1\uD55C \uC124\uCE58 \uC5C6\uC774 \uC2E4\uD589\uD558\
  \uACE0 \uC2F6\uB2E4\uBA74, `<chrono>` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB9CC \uC0AC\
  \uC6A9\uD574\uB3C4 \uB300\uBD80\uBD84\uC758 \uC77C\uBC18\uC801\uC778 \uB0A0\uC9DC\
  \ \uACC4\uC0B0\uC774 \uAC00\uB2A5\uD569\uB2C8\uB2E4."
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
weight: 26
---

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
