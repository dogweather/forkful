---
date: 2024-01-20 17:32:41.599744-07:00
description: "How to: (\uC2EC\uCE35 \uD0D0\uAD6C) \uACFC\uAC70\uC5D0\uB294 `ctime`\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC758 `tm` \uAD6C\uC870\uCCB4\uB97C \uC774\uC6A9\
  \uD558\uC5EC \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD588\uC2B5\uB2C8\uB2E4. \uD604\uC7AC\
  \uB294 C++11\uBD80\uD130 \uB3C4\uC785\uB41C `<chrono>` \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uAC00 \uC2DC\uAC04 \uAD00\uB828 \uC791\uC5C5\uC5D0 \uC0C8\uB85C\uC6B4 \uD45C\uC900\
  \uC73C\uB85C \uC790\uB9AC\uC7A1\uC558\uC2B5\uB2C8\uB2E4. `<chrono>`\uB294 \uB354\
  \ \uC815\uD655\uD558\uACE0 \uD604\uB300\uC801\uC778 \uC2DC\uAC04\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.314031-06:00'
model: gpt-4-1106-preview
summary: "(\uC2EC\uCE35 \uD0D0\uAD6C) \uACFC\uAC70\uC5D0\uB294 `ctime` \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uC758 `tm` \uAD6C\uC870\uCCB4\uB97C \uC774\uC6A9\uD558\uC5EC\
  \ \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD588\uC2B5\uB2C8\uB2E4."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
weight: 27
---

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
