---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:14.240113-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: C++11\uC740 `<regex>`\uB77C\uB294 \uD45C\uC900\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\uC2DD\uC5D0\
  \ \uB300\uD55C \uC9C0\uC6D0\uC744 \uB3C4\uC785\uD558\uC5EC, \uBB38\uC790\uC5F4 \uAC80\
  \uC0C9 \uBC0F \uC870\uC791\uC744 \uC704\uD55C \uACAC\uACE0\uD55C \uD504\uB808\uC784\
  \uC6CC\uD06C\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uBB38\uC790\
  \uC5F4 \uB0B4\uC5D0\uC11C \uD328\uD134\uC744 \uAC80\uC0C9\uD558\uAE30 \uC704\uD574\
  \ \uC815\uADDC \uD45C\uD604\uC2DD\uC744 \uC0AC\uC6A9\uD558\uB294 \uAE30\uBCF8 \uC608\
  \uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.654394-06:00'
model: gpt-4-0125-preview
summary: "C++11\uC740 `<regex>`\uB77C\uB294 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uC5D0\uC11C \uC815\uADDC \uD45C\uD604\uC2DD\uC5D0 \uB300\uD55C \uC9C0\uC6D0\uC744\
  \ \uB3C4\uC785\uD558\uC5EC, \uBB38\uC790\uC5F4 \uAC80\uC0C9 \uBC0F \uC870\uC791\uC744\
  \ \uC704\uD55C \uACAC\uACE0\uD55C \uD504\uB808\uC784\uC6CC\uD06C\uB97C \uC81C\uACF5\
  \uD569\uB2C8\uB2E4."
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
weight: 11
---

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
