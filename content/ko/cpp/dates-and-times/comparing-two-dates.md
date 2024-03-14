---
date: 2024-01-20 17:32:41.599744-07:00
description: "(\uBB34\uC5C7\uACFC \uC65C?) \uB450 \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD55C\
  \uB2E4\uB294 \uAC83\uC740 \uB0A0\uC9DC A\uAC00 B\uBCF4\uB2E4 \uC774\uC804, \uB3D9\
  \uC77C, \uB610\uB294 \uC774\uD6C4\uC778\uC9C0\uB97C \uD655\uC778\uD558\uB294 \uACFC\
  \uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uC774\uB97C\
  \ \uC218\uD589\uD558\uB294 \uC774\uC720\uB294 \uC720\uD6A8\uC131 \uAC80\uC0AC, \uC2DC\
  \uAC04\uCC28 \uACC4\uC0B0, \uC77C\uC815 \uAD00\uB9AC \uB4F1\uC744 \uC704\uD574\uC11C\
  \uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.687277-06:00'
model: gpt-4-1106-preview
summary: "(\uBB34\uC5C7\uACFC \uC65C?) \uB450 \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD55C\
  \uB2E4\uB294 \uAC83\uC740 \uB0A0\uC9DC A\uAC00 B\uBCF4\uB2E4 \uC774\uC804, \uB3D9\
  \uC77C, \uB610\uB294 \uC774\uD6C4\uC778\uC9C0\uB97C \uD655\uC778\uD558\uB294 \uACFC\
  \uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uC774\uB97C\
  \ \uC218\uD589\uD558\uB294 \uC774\uC720\uB294 \uC720\uD6A8\uC131 \uAC80\uC0AC, \uC2DC\
  \uAC04\uCC28 \uACC4\uC0B0, \uC77C\uC815 \uAD00\uB9AC \uB4F1\uC744 \uC704\uD574\uC11C\
  \uC785\uB2C8\uB2E4."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why?
(무엇과 왜?)
두 날짜를 비교한다는 것은 날짜 A가 B보다 이전, 동일, 또는 이후인지를 확인하는 과정입니다. 프로그래머들이 이를 수행하는 이유는 유효성 검사, 시간차 계산, 일정 관리 등을 위해서입니다.

## How to:
(어떻게:)
```C++
#include <iostream>
#include <chrono>

int main() {
    using namespace std::chrono;

    // 오늘 날짜와 임의의 날짜 두 개를 system_clock을 활용하여 정의합니다.
    system_clock::time_point today = system_clock::now();
    system_clock::time_point arbitrary_date1 = system_clock::now() - hours(24); // 하루 전
    system_clock::time_point arbitrary_date2 = system_clock::now() + hours(24); // 하루 후

    // 오늘과 비교
    if (arbitrary_date1 < today) {
        std::cout << "arbitrary_date1은 오늘보다 이전입니다." << std::endl;
    }
    if (arbitrary_date2 > today) {
        std::cout << "arbitrary_date2는 오늘보다 이후입니다." << std::endl;
    }
}
```

Sample Output:
```
arbitrary_date1은 오늘보다 이전입니다.
arbitrary_date2는 오늘보다 이후입니다.
```

## Deep Dive:
(심층 탐구)
과거에는 `ctime` 라이브러리의 `tm` 구조체를 이용하여 날짜를 비교했습니다. 현재는 C++11부터 도입된 `<chrono>` 라이브러리가 시간 관련 작업에 새로운 표준으로 자리잡았습니다. `<chrono>`는 더 정확하고 현대적인 시간 계산을 가능케 하며, 타임 존이나 DST(Daylight Saving Time) 같은 복잡한 문제를 다루기 좋습니다. 하지만 아직 장점들이 활용되지 않는 코드를 보는 것도 흔합니다. 다른 방법으로는 타임스탬프를 비교하는 방식이 있지만, `<chrono>`가 더 직관적이고 세련된 방법을 제공합니다.

## See Also:
(더보기)
- [cppreference.com: <chrono>](https://en.cppreference.com/w/cpp/header/chrono)
- [cppreference.com: Time point comparisons](https://en.cppreference.com/w/cpp/chrono/time_point)
- [ISO C++: Date and time utilities](https://isocpp.org/wiki/faq/cpp-library#chrono)
- [Stack Overflow: Comparing dates in C++](https://stackoverflow.com/questions/tagged/c%2b%2b+date+comparison)
