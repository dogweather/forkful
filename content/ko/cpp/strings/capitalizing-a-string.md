---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:15.275952-07:00
description: "\uBC29\uBC95: C++\uC5D0\uC11C\uB294 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\
  \uB97C \uB300\uBB38\uC790\uB85C \uB9CC\uB4E4 \uC218 \uC788\uC73C\uBA70, \uC81C3\uC790\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uD544\uC694\uD558\uC9C0 \uC54A\uC2B5\uB2C8\
  \uB2E4. \uADF8\uB7EC\uB098 \uC880 \uB354 \uBCF5\uC7A1\uD558\uAC70\uB098 \uD2B9\uC815\
  \uD55C \uB300\uBB38\uC790\uD654 \uD589\uB3D9\uC774 \uD544\uC694\uD55C \uACBD\uC6B0\
  \uC5D0\uB294 Boost \uAC19\uC740 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uB9E4\uC6B0\
  \ \uC720\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC544\uB798\uB294 \uB450\
  \ \uC811\uADFC\uBC95\uC744\u2026"
lastmod: '2024-03-13T22:44:55.644353-06:00'
model: gpt-4-0125-preview
summary: "C++\uC5D0\uC11C\uB294 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\
  \uBB38\uC790\uB85C \uB9CC\uB4E4 \uC218 \uC788\uC73C\uBA70, \uC81C3\uC790 \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uAC00 \uD544\uC694\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

## 방법:
C++에서는 표준 라이브러리를 사용하여 문자열의 첫 글자를 대문자로 만들 수 있으며, 제3자 라이브러리가 필요하지 않습니다. 그러나 좀 더 복잡하거나 특정한 대문자화 행동이 필요한 경우에는 Boost 같은 라이브러리가 매우 유용할 수 있습니다. 아래는 두 접근법을 보여주는 예시입니다.

### 표준 C++ 라이브러리 사용:
```cpp
#include <iostream>
#include <cctype> // std::tolower 와 std::toupper를 위해
#include <string>

std::string capitalizeString(const std::string& input) {
    std::string result;
    bool capitalizeNext = true;

    for (char ch : input) {
        if (std::isspace(ch)) {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            ch = std::toupper(ch);
            capitalizeNext = false;
        }
        result += ch;
    }

    return result;
}

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl; // 출력: "Hello World From C++"
}
```

### Boost 라이브러리 사용:
보다 고급 문자열 조작을 위해, 로케일을 고려한 대문자화를 포함하여, Boost String Algo 라이브러리를 사용할 수 있습니다.

먼저 프로젝트에 Boost 라이브러리가 설치되어 있고 구성되어 있는지 확인하세요. 그런 다음 필요한 헤더를 포함하고 아래와 같이 그 기능을 사용할 수 있습니다.

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = text;

    // 각 단어의 첫 글자를 대문자로
    boost::algorithm::to_lower(capitalizedText); // 문자열을 소문자로 변환하여 확실하게 합니다.
    capitalizedText[0] = std::toupper(capitalizedText[0]); // 첫 문자를 대문자로 변환합니다.

    for (std::size_t i = 1; i < capitalizedText.length(); ++i) {
        if (isspace(capitalizedText[i - 1])) { // 공백 뒤에 대문자로
            capitalizedText[i] = std::toupper(capitalizedText[i]);
        }
    }

    std::cout << capitalizedText << std::endl; // 출력: "Hello World From C++"
}
```

이 경우, Boost는 일부 문자열 조작 작업을 단순화하지만, 변환 및 대소문자 전환 유틸리티를 주로 제공하기 때문에 진정한 대문자화를 위해서는 여전히 맞춤 접근 방식이 필요합니다.
