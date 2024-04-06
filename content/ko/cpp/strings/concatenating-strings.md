---
date: 2024-01-20 17:34:06.573203-07:00
description: "How to: (\uBC29\uBC95) \uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uCC98\uC74C\
  \ C\uAC00 \uB4F1\uC7A5\uD588\uC744 \uB54C\uBD80\uD130 \uAE30\uBCF8\uC801\uC778 \uAE30\
  \uB2A5\uC774\uC5C8\uC2B5\uB2C8\uB2E4. C++\uC5D0\uC11C\uB294 `std::string`\uC774\
  \ \uC774\uB97C \uC27D\uAC8C \uD574\uC8FC\uBA70, `+` \uC5F0\uC0B0\uC790\uB97C \uC774\
  \uC6A9\uD569\uB2C8\uB2E4. \uC5EC\uB7EC \uBC29\uBC95(\uC608: `append()`, `stringstream`,\
  \ `fmt` \uB77C\uC774\uBE0C\uB7EC\uB9AC)\uC73C\uB85C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.291835-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uCC98\uC74C C\uAC00\
  \ \uB4F1\uC7A5\uD588\uC744 \uB54C\uBD80\uD130 \uAE30\uBCF8\uC801\uC778 \uAE30\uB2A5\
  \uC774\uC5C8\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

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
