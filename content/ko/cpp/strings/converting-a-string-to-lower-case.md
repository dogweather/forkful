---
date: 2024-01-20 17:37:54.067759-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uC704 \uCF54\uB4DC\
  \uB294 \"Hello World!\" \uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\
  \uD658\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.286571-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uC704 \uCF54\uB4DC\uB294 \"Hello\
  \ World!\" \uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD569\uB2C8\
  \uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 4
---

## How to: (어떻게 하나요?)
```C++
#include <iostream>
#include <algorithm>
#include <cctype>
#include <string>

int main() {
    std::string data = "Hello World!";
    std::transform(data.begin(), data.end(), data.begin(),
                   [](unsigned char c){ return std::tolower(c); });

    std::cout << data << std::endl;  // "hello world!"
    return 0;
}
```
위 코드는 "Hello World!" 문자열을 소문자로 변환합니다.

## Deep Dive (심화 학습)
과거 C++에서는 `std::tolower`를 문자에 직접 적용하거나 C 스타일로 각 글자를 순회하면서 변환하는 방법을 사용했습니다. 최신 C++에서는 `<algorithm>` 헤더의 `std::transform`을 사용하여 람다 함수로 한 줄에 간단히 변환할 수 있습니다. 람다 함수 내에서는 `std::tolower`가 각 문자를 소문자로 변환합니다. `std::tolower`를 사용할 때는 `unsigned char`로 형변환하는 것을 잊지 마세요. 잘못된 사용으로 인한 오류를 피하기 위함입니다.

대안으로, Boost 라이브러리나 C++17의 `<locale>`를 사용한 방법도 있지만, 일반적인 용도에는 `std::transform`과 `std::tolower`의 조합이 간단하고 효율적입니다.

내부 구현에 관해서는, `std::tolower`는 로케일에 의존적입니다. 기본적으로 현재 로케일의 규칙을 따라 소문자 변환이 이루어지지만, 로케일을 지정하지 않으면 C 로케일을 사용합니다. 이것은 다국어 문자열 처리에서는 주의가 필요한 부분입니다.

## See Also (추가 정보)
- C++ Reference for `std::transform`: https://en.cppreference.com/w/cpp/algorithm/transform
- C++ Reference for `std::tolower`: https://en.cppreference.com/w/cpp/string/byte/tolower
- Boost String Algorithms Library: https://www.boost.org/doc/libs/release/libs/algorithm/string/
- C++ Locale library for internationalization: https://en.cppreference.com/w/cpp/locale
