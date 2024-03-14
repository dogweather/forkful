---
date: 2024-01-20 17:34:06.573203-07:00
description: "\uBB38\uC790\uC5F4 \uC5F0\uACB0(concatenation)\uC740 \uBB38\uC790\uC5F4\
  \uC744 \uC774\uC5B4 \uBD99\uC774\uB294 \uAC83\uC785\uB2C8\uB2E4. \uB370\uC774\uD130\
  \uB97C \uD569\uCE58\uAC70\uB098 \uCD9C\uB825\uC744 \uD615\uC2DD\uD654\uD560 \uB54C\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.656651-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4 \uC5F0\uACB0(concatenation)\uC740 \uBB38\uC790\uC5F4\uC744\
  \ \uC774\uC5B4 \uBD99\uC774\uB294 \uAC83\uC785\uB2C8\uB2E4. \uB370\uC774\uD130\uB97C\
  \ \uD569\uCE58\uAC70\uB098 \uCD9C\uB825\uC744 \uD615\uC2DD\uD654\uD560 \uB54C \uC0AC\
  \uC6A9\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 연결(concatenation)은 문자열을 이어 붙이는 것입니다. 데이터를 합치거나 출력을 형식화할 때 사용합니다.

## How to: (방법)
```C++
#include <iostream>
#include <string>

int main() {
    std::string hello = "안녕, ";
    std::string world = "세계!";
    std::string greeting = hello + world; // 문자열 연결

    std::cout << greeting << std::endl; // "안녕, 세계!" 출력

    return 0;
}
```

## Deep Dive (심층 탐구)
문자열 연결은 처음 C가 등장했을 때부터 기본적인 기능이었습니다. C++에서는 `std::string`이 이를 쉽게 해주며, `+` 연산자를 이용합니다. 여러 방법(예: `append()`, `stringstream`, `fmt` 라이브러리)으로 문자열을 연결할 수 있습니다. `+` 연산자는 내부적으로 `append()`를 호출하여 문자열을 연결합니다. 큰 데이터를 다룰 때는 `+` 대신 `append()`를 사용하면 성능이 향상됩니다.

## See Also (관련 링크)
- C++ Reference `std::string`: https://en.cppreference.com/w/cpp/string/basic_string
- fmt 라이브러리 문서: https://fmt.dev/latest/index.html
- C++ `stringstream`: https://www.cplusplus.com/reference/sstream/stringstream/
