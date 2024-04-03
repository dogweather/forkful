---
date: 2024-01-26 03:43:45.708468-07:00
description: "\uBC29\uBC95: C++\uB294 `floor()`, `ceil()`, `round()`\uC640 \uAC19\uC774\
  \ \uC22B\uC790\uB97C \uBC18\uC62C\uB9BC\uD558\uB294 \uC5EC\uB7EC \uAC00\uC9C0 \uBC29\
  \uBC95\uC744 \uC81C\uACF5\uD55C\uB2E4."
lastmod: '2024-03-13T22:44:55.660704-06:00'
model: gpt-4-0125-preview
summary: "C++\uB294 `floor()`, `ceil()`, `round()`\uC640 \uAC19\uC774 \uC22B\uC790\
  \uB97C \uBC18\uC62C\uB9BC\uD558\uB294 \uC5EC\uB7EC \uAC00\uC9C0 \uBC29\uBC95\uC744\
  \ \uC81C\uACF5\uD55C\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
weight: 13
---

## 방법:
C++는 `floor()`, `ceil()`, `round()`와 같이 숫자를 반올림하는 여러 가지 방법을 제공한다:

```C++
#include <iostream>
#include <cmath> // 반올림 함수를 위해

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // 출력: floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // 출력: ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // 출력: round: 3

    // 소수점 둘째 자리까지 반올림하는 경우:
    double precise_num = 3.146;
    double multiplier = 100.0;
    double rounded = std::round(precise_num * multiplier) / multiplier;

    std::cout << "소수점 둘째 자리까지 반올림: " << rounded << "\n"; // 출력: 소수점 둘째 자리까지 반올림: 3.15

    return 0;
}
```

## 심층 분석
C++11 이전에는 수동 기술이나 비표준 라이브러리에 의존하여 반올림이 이루어졌다. 오늘날, `<cmath>`는 견고한 방법을 제공한다. `floor()`는 내림, `ceil()`는 올림, 반면 `round()`는 가장 가까운 정수로 반올림하는데, 타이브레이킹(0.5 경우)도 처리하여 짝수로 반올림한다.

이 함수들의 동작을 이해하는 것은 중요하다; 예를 들어, 음수는 당신을 혼란에 빠뜨릴 수 있다(`std::round(-2.5)`는 `-2.0`을 반환).

대안? 양의 수에 대해 0.5를 더한 후 int로 캐스팅하는 것은 고전적인 해킹이었지만, 음수에서 오류가 발생하며 타입에 구애받지 않는 것이 아니다. Boost와 같은 라이브러리는 보다 미묘한 접근 방식을 제공할 수 있으며, 언어 확장이나 컴파일러 내장 함수는 특정 하드웨어에 대해 최적화할 수 있다.

## 참고 자료
- `<cmath>`에 대한 C++ 참조: https://en.cppreference.com/w/cpp/header/cmath
- 부동 소수점 산술을 위한 IEEE 표준 (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Boost 수치 변환 라이브러리: https://www.boost.org/doc/libs/release/libs/numeric/conversion/
