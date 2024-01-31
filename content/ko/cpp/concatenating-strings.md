---
title:                "문자열 연결하기"
date:                  2024-01-20T17:34:06.573203-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 연결하기"

category:             "C++"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/concatenating-strings.md"
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
