---
title:                "정규 표현식 활용하기"
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? [무엇이며, 왜 사용하는가?]
정규 표현식은 문자열 패턴을 찾기 위한 강력한 도구입니다. 프로그래머는 코드를 단순화하고, 유효성 검사나 문자열 분석을 효율적으로 하기 위해 사용합니다.

## How to: [사용 방법]
```C++
#include <iostream>
#include <regex>

int main() {
    std::string text = "The C++ convention starts on 2023-04-05.";
    std::regex date_pattern(R"(\d{4}-\d{2}-\d{2})"); // YYYY-MM-DD 포맷

    std::smatch matches;
    if (std::regex_search(text, matches, date_pattern)) {
        std::cout << "Found date: " << matches[0] << std::endl;
    }

    return 0;
}
```
출력:
```
Found date: 2023-04-05
```

## Deep Dive [심층 분석]
- **역사적 맥락**: 정규 표현식은 1950년대부터 존재했고, Perl 등의 언어에서 대중화되었습니다.
- **대안들**: 문자열 검색에는 `std::find` 또는 `std::string` 메서드들이 있지만, 정무 표현식처럼 강력하지 않습니다.
- **구현 세부사항**: C++에는 `<regex>` 헤더를 통해 정규 표현식 라이브러리가 내장되어 있습니다. `std::regex` 객체를 사용해 패턴을 정의하고, `std::regex_search`로 검색합니다.

## See Also [추가 정보]
- C++ `<regex>` library: https://en.cppreference.com/w/cpp/regex
- 정규 표현식 입문: https://www.regular-expressions.info/tutorial.html
- 정규 표현식을 연습할 수 있는 온라인 툴: https://regex101.com/
