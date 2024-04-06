---
date: 2024-01-20 17:45:11.315845-07:00
description: "How to (\uBC29\uBC95) `std::string` \uC548\uC5D0 \uC788\uB294 `substr`\
  \ \uD568\uC218\uB294 C++98\uBD80\uD130 \uC0AC\uC6A9\uB418\uC5B4 \uC654\uC2B5\uB2C8\
  \uB2E4. \uBB38\uC790\uC5F4\uC744 \uB2E4\uB8F0 \uB54C \uAE30\uBCF8\uC801\uC73C\uB85C\
  \ \uC81C\uACF5\uD558\uB294 \uAC15\uB825\uD55C \uAE30\uB2A5 \uC911 \uD558\uB098\uC8E0\
  . `substr` \uD568\uC218\uB294 \uB450 \uAC1C\uC758 \uB9E4\uAC1C\uBCC0\uC218\uB97C\
  \ \uBC1B\uC544 \uCCAB \uBC88\uC9F8 \uB9E4\uAC1C\uBCC0\uC218\uB294 \uC2DC\uC791 \uC778\
  \uB371\uC2A4\uB97C, \uB450 \uBC88\uC9F8\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.288309-06:00'
model: gpt-4-1106-preview
summary: "\uBB38\uC790\uC5F4\uC744 \uB2E4\uB8F0 \uB54C \uAE30\uBCF8\uC801\uC73C\uB85C\
  \ \uC81C\uACF5\uD558\uB294 \uAC15\uB825\uD55C \uAE30\uB2A5 \uC911 \uD558\uB098\uC8E0\
  ."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C"
weight: 6
---

## How to (방법)
```C++
#include <iostream>
#include <string>

int main() {
    std::string fullText = "Hello, C++ World!";
    std::string subText = fullText.substr(7, 3); // "C++" 추출

    std::cout << "Original string: " << fullText << std::endl;
    std::cout << "Extracted substring: " << subText << std::endl;

    return 0;
}
```
출력:
```
Original string: Hello, C++ World!
Extracted substring: C++
```

## Deep Dive (깊이 탐구)
`std::string` 안에 있는 `substr` 함수는 C++98부터 사용되어 왔습니다. 문자열을 다룰 때 기본적으로 제공하는 강력한 기능 중 하나죠. `substr` 함수는 두 개의 매개변수를 받아 첫 번째 매개변수는 시작 인덱스를, 두 번째 매개변수는 추출할 문자의 길이를 지정합니다. 

다른 방법도 있습니다. C++17부터는 `std::string_view`, 한정된 메모리 사용으로 문자열을 보다 효율적으로 다루게 도와주죠. 이것을 사용해도 문자열의 부분을 취급할 수 있습니다.

구현 세부사항을 살펴보면, `substr`은 새로운 문자열을 만들기 때문에 메모리를 할당하고 이전 문자열로부터 데이터를 복사합니다. 이것은 큰 문자열이나 빈번한 작업에 대해 성능 저하를 일으킬 수 있습니다.

## See Also (참고 자료)
- C++ Reference for `std::string::substr`: https://en.cppreference.com/w/cpp/string/basic_string/substr
- C++ `std::string_view`: https://en.cppreference.com/w/cpp/string/basic_string_view
- C++ Standard Library Reference: https://cplusplus.com/reference/
