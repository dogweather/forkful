---
date: 2024-01-20 17:50:32.799381-07:00
description: "How to (\uBC29\uBC95) C++\uC5D0\uC11C\uB294 `std::stringstream`, `std::format`\
  \ (C++20\uBD80\uD130 \uC0AC\uC6A9 \uAC00\uB2A5) \uB4F1\uC744 \uC0AC\uC6A9\uD558\uC5EC\
  \ \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC744 \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4.\
  \ \uC5EC\uAE30 \uB450 \uAC00\uC9C0 \uC608\uC2DC\uAC00 \uC788\uC2B5\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.285707-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## How to (방법)
C++에서는 `std::stringstream`, `std::format` (C++20부터 사용 가능) 등을 사용하여 문자열 보간을 할 수 있습니다. 여기 두 가지 예시가 있습니다:

```C++
#include <iostream>
#include <sstream>
#include <string>
#include <format>

int main() {
    // Using stringstream
    std::stringstream ss;
    int age = 25;
    ss << "나이: " << age << "세";
    std::cout << ss.str() << std::endl; // Output: 나이: 25세
    
    // Using std::format (C++20)
    std::string name = "홍길동";
    std::cout << std::format("이름: {}", name) << std::endl; // Output: 이름: 홍길동

    return 0;
}
```

## Deep Dive (심층 분석)
문자열 보간은 오래 전부터 다양한 프로그래밍 언어에서 사용되어 왔습니다. C++에서는 오랫동안 `sprintf`와 같은 C 스타일의 함수를 사용했습니다. 하지만 C++11에서 `std::stringstream`이 등장했으며, 더 최근 C++20에서는 `std::format`을 도입하여 더 쉬운 문자열 보간을 가능하게 했습니다.

`std::format`은 Python의 `str.format()`과 유사한 문법을 제공합니다. 이는 타입 안전성과 가독성을 크게 개선합니다. 다만 이 기능을 사용하려면 C++20 이상을 지원하는 컴파일러가 필요합니다.

`std::stringstream`은 보다 오래된 방법이지만, 모든 현대 C++ 컴파일러에서 사용할 수 있습니다. 그리고 문자열을 조립하는 방식에서 유연성을 제공합니다.

## See Also (추가 정보)
- [cppreference.com - std::format](https://en.cppreference.com/w/cpp/utility/format)
- [cppreference.com - std::stringstream](https://en.cppreference.com/w/cpp/io/basic_stringstream)
- [Wikipedia - String interpolation](https://en.wikipedia.org/wiki/String_interpolation)
