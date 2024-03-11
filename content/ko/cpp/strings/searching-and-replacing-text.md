---
date: 2024-01-20 17:57:15.553103-07:00
description: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\uB294 \uBB38\uC790\
  \uC5F4\uC5D0\uC11C \uD2B9\uC815\uB2E8\uC5B4\uB97C \uCC3E\uC544 \uB2E4\uB978 \uB2E8\
  \uC5B4\uB85C \uBC14\uAFB8\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uC218\uC815, \uCF54\uB4DC \uB9AC\uD329\
  \uD1A0\uB9C1, \uC790\uB3D9\uD654\uB41C \uC77C\uAD04 \uCC98\uB9AC \uB4F1\uC744 \uC704\
  \uD574 \uC774 \uAE30\uB2A5\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:29.574023-06:00'
model: gpt-4-1106-preview
summary: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\uB294 \uBB38\uC790\uC5F4\
  \uC5D0\uC11C \uD2B9\uC815\uB2E8\uC5B4\uB97C \uCC3E\uC544 \uB2E4\uB978 \uB2E8\uC5B4\
  \uB85C \uBC14\uAFB8\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uC218\uC815, \uCF54\uB4DC \uB9AC\uD329\uD1A0\
  \uB9C1, \uC790\uB3D9\uD654\uB41C \uC77C\uAD04 \uCC98\uB9AC \uB4F1\uC744 \uC704\uD574\
  \ \uC774 \uAE30\uB2A5\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 검색 및 교체는 문자열에서 특정단어를 찾아 다른 단어로 바꾸는 과정입니다. 프로그래머들은 데이터 수정, 코드 리팩토링, 자동화된 일괄 처리 등을 위해 이 기능을 사용합니다.

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
