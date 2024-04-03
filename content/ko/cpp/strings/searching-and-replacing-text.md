---
date: 2024-01-20 17:57:15.553103-07:00
description: "How to: (\uBC29\uBC95) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.647462-06:00'
model: gpt-4-1106-preview
summary: .
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

## How to: (방법)
```C++
#include <iostream>
#include <string>
#include <regex>

int main() {
    std::string text = "안녕하세요. 이 문장 안에 있는 말을 바꿉니다: 사과.";
    std::regex word_to_replace("사과");
    std::cout << "Before:\n" << text << std::endl;

    text = std::regex_replace(text, word_to_replace, "오렌지");

    std::cout << "After:\n" << text << std::endl;
    return 0;
}
```
Sample Output:
```
Before:
안녕하세요. 이 문장 안에 있는 말을 바꿉니다: 사과.
After:
안녕하세요. 이 문장 안에 있는 말을 바꿉니다: 오렌지.
```

## Deep Dive (심층 탐구)
C++은 STL(Standard Template Library)의 `std::string` 클래스를 사용하여 문자열을 관리합니다. 과거 C 스타일의 문자 배열을 사용한 뒤 `std::string`으로 옮겨갔습니다.

규칙기반 텍스트 변경에는 `std::regex` 클래스를 사용합니다. 찾고자 하는 패턴을 정규 표현식으로 정의하고 `std::regex_replace` 함수로 대체합니다. 

대안으로는 문자열의 `find()`와 `replace()`를 사용할 수 있지만, 정규 표현식을 사용하면 복잡한 패턴 매칭에 유용합니다.

C++11 이전에는 정규 표현식을 직접 구현해야 했지만, 이후 표준 라이브러리에 통합되어 사용하기가 훨씬 쉬워졌습니다.

## See Also (추가 정보)
- C++ Reference for std::regex: https://en.cppreference.com/w/cpp/regex
- C++ Reference for std::string: http://www.cplusplus.com/reference/string/string/
- Regex tester and debugger: https://regex101.com/
