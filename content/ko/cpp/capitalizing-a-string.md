---
title:                "문자열 대문자화"
html_title:           "C++: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 그렇게 하는가?
문자열 대문자 변경에는 문자열에서 각 단어의 첫 글자를 대문자로 변경하는 과정이 포함됩니다. 프로그래머들이 이 작업을 수행하는 주요한 이유는 사용자의 입력값을 표준화하고 사용자가 의도하지 않게 입력한 소문자/대문자를 수정하여 데이터의 일관성을 유지하기 위함입니다.

## 어떻게 하는가:
```C++
#include <algorithm>
#include <cctype>
#include <iostream>

int main() {
    std::string str = "hello world";
    std::transform(str.begin(), str.end(), str.begin(),
        [](unsigned char c){ return std::toupper(c); });
    std::cout << str;
    return 0;
}
```
출력 결과:
```C++
HELLO WORLD
```

## 디프 다이브
**역사적 맥락**: 문자열 대문자 변경은 컴퓨팅의 초기 시절부터 존재하여 사용자 입력의 표준화와 가독성을 향상 시키는 데 사용되었습니다.

**대안**: C++에서는 대문자 변경 라이브러리 외에도 Boost, POCO와 같은 서드파티 라이브러리를 사용하여 문자열 대문자 변경을 수행할 수 있습니다.

**구현 세부 정보**: `std::transform`은 C++ Standard Library의 일부로, 주어진 범위의 모든 요소에 연산을 적용하고, 그 결과를 다른 범위로 반환합니다. 이 경우에는 `std::toupper`를 각 문자에 적용하여 결과 문자열을 반환합니다.

## 참고하기
- C++ Reference(`std::transform`): [https://en.cppreference.com/w/cpp/algorithm/transform](https://en.cppreference.com/w/cpp/algorithm/transform)
- C++ Reference(`std::toupper`): [https://en.cppreference.com/w/cpp/string/byte/toupper](https://en.cppreference.com/w/cpp/string/byte/toupper)
- Boost String Algorithms Library: [https://www.boost.org/doc/libs/1_71_0/doc/html/string_algo.html](https://www.boost.org/doc/libs/1_71_0/doc/html/string_algo.html)
- POCO String Manipulation: [https://pocoproject.org/docs/Poco.String.html](https://pocoproject.org/docs/Poco.String.html)