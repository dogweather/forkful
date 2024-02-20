---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:15.275952-07:00
description: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\
  \uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC740 \uD574\uB2F9 \uBB38\uC790\uC5F4\uC758\
  \ \uAC01 \uB2E8\uC5B4\uC758 \uCCAB \uBB38\uC790\uB97C \uC18C\uBB38\uC790\uC778 \uACBD\
  \uC6B0 \uB300\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uBA74\uC11C \uB098\uBA38\uC9C0\
  \ \uBB38\uC790\uB294 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uB294 \uACFC\uC815\uC744 \uD3EC\
  \uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885\
  \ \uCD9C\uB825 \uD615\uC2DD, \uC0AC\uC6A9\uC790 \uC785\uB825 \uB610\uB294 \uB370\
  \uC774\uD130 \uCC98\uB9AC\uB97C \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\
  \uD558\uC5EC \uD14D\uC2A4\uD2B8\uAC00 \uD45C\uC2DC\uB418\uAC70\uB098\u2026"
lastmod: 2024-02-19 22:05:14.557966
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C\
  \ \uBCC0\uD658\uD558\uB294 \uAC83\uC740 \uD574\uB2F9 \uBB38\uC790\uC5F4\uC758 \uAC01\
  \ \uB2E8\uC5B4\uC758 \uCCAB \uBB38\uC790\uB97C \uC18C\uBB38\uC790\uC778 \uACBD\uC6B0\
  \ \uB300\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uBA74\uC11C \uB098\uBA38\uC9C0 \uBB38\
  \uC790\uB294 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uB294 \uACFC\uC815\uC744 \uD3EC\uD568\
  \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885 \uCD9C\
  \uB825 \uD615\uC2DD, \uC0AC\uC6A9\uC790 \uC785\uB825 \uB610\uB294 \uB370\uC774\uD130\
  \ \uCC98\uB9AC\uB97C \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uC5EC\
  \ \uD14D\uC2A4\uD2B8\uAC00 \uD45C\uC2DC\uB418\uAC70\uB098\u2026"
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열의 첫 글자를 대문자로 변환하는 것은 해당 문자열의 각 단어의 첫 문자를 소문자인 경우 대문자로 변환하면서 나머지 문자는 변경하지 않는 과정을 포함합니다. 프로그래머들은 종종 출력 형식, 사용자 입력 또는 데이터 처리를 위해 이 작업을 수행하여 텍스트가 표시되거나 처리되는 방식의 일관성을 보장합니다. 특히 사용자 인터페이스나 데이터 정규화 작업에서 그렇습니다.

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
