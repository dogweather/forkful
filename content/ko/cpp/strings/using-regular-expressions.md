---
title:                "정규 표현식 사용하기"
aliases:
- ko/cpp/using-regular-expressions.md
date:                  2024-02-03T19:16:14.240113-07:00
model:                 gpt-4-0125-preview
simple_title:         "정규 표현식 사용하기"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜 사용하는가?
C++에서 정규 표현식은 검색 패턴을 정의하는 문자의 연속으로, 문자열 일치나 조작에 사용됩니다. 프로그래머는 입력 검증, 문자열 내 발생하는 항목 검색, 문자열을 토큰으로 분리하는 등의 작업에 정규 표현식을 사용하여, 효율적이고 효과적인 텍스트 처리에 필수적인 도구로 삼습니다.

## 사용 방법:
C++11은 `<regex>`라는 표준 라이브러리에서 정규 표현식에 대한 지원을 도입하여, 문자열 검색 및 조작을 위한 견고한 프레임워크를 제공합니다. 다음은 문자열 내에서 패턴을 검색하기 위해 정규 표현식을 사용하는 기본 예입니다:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string target = "Hello, my email is example@example.com";
    std::regex email_pattern(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b)");

    if (std::regex_search(target, email_pattern)) {
        std::cout << "이메일 찾음!" << std::endl;
    } else {
        std::cout << "이메일 없음." << std::endl;
    }

    return 0;
}
```
**출력 예시**
```
이메일 찾음!
```

문자열 내 패턴을 대체하는 등 좀 더 복잡한 조작이 필요한 경우, C++의 정규 표현식은 매우 유용할 수 있습니다:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string text = "The rain in Spain falls mainly in the plain.";
    std::regex vowel_regex("([aeiou])");

    std::string replaced_text = std::regex_replace(text, vowel_regex, "*");
    std::cout << replaced_text << std::endl;

    return 0;
}
```
**출력 예시**
```
Th* r**n *n Sp**n f*lls m**nly *n th* pl**n.
```

표준 라이브러리를 넘어서 탐색하는 프로그래머를 위해, Boost Regex 라이브러리(`boost/regex.hpp`)는 특히 복잡한 패턴이나 대량의 데이터 처리에 대한 향상된 정규 표현식 기능과 성능 최적화를 제공하는 인기 있는 제3자 옵션입니다:

```cpp
#include <iostream>
#include <boost/regex.hpp>

int main() {
    std::string s = "Boost libraries are fun!";
    boost::regex expr("(\\w+)\\s(libraries)"); // "Boost libraries"와 일치
    std::string fmt("GNU \\1"); // "GNU Boost"로 대체

    std::string result = boost::regex_replace(s, expr, fmt);
    std::cout << result << std::endl;

    return 0;
}
```
**출력 예시**
```
GNU Boost는 재미있습니다!
```

이 예제들은 C++의 정규 표현식으로 가능한 기능들을 간략하게 보여주며, 표준 라이브러리를 사용하든 Boost의 강력한 정규 표현식 구현을 활용하든 기본적인 검색, 패턴 일치, 대체 등을 보여줍니다.
