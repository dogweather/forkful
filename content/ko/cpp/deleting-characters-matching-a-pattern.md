---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 그럴까?

패턴에 일치하는 문자를 삭제하는 것은 특정 패턴이나 문자열이 코드 내에 중복되거나 필요하지 않게 되었을 때, 그것을 제거하는 작업을 말합니다. 이는 코드의 효율성과 가독성을 향상시키기 위해 필요한 작업입니다.

## 어떻게 하나요?

다음은 C++의 `regex_replace` 함수를 사용하여 문자열에서 패턴이 일치하는 문자를 제거하는 방법입니다.

```c++
#include <regex>
#include <iostream>

int main() {
    std::string str = "Hello, World!";
    std::string pattern = "[aeiou]";
    std::regex r(pattern, std::regex_constants::icase);
    std::string result = std::regex_replace(str, r, "");

    std::cout << result << std::endl; // 출력: "Hll, Wrld!"
    return 0;
}
```

`regex_replace` 함수는 주어진 정규 표현식과 일치하는 문자를 제거(혹은 바꿈)하여 결과적으로 "Hello, World!"에서 모든 모음이 제거된 "Hll, Wrld!"를 출력함을 확인할 수 있습니다.

## 깊이 들어가서 보기

문자열에서 패턴에 일치하는 문자를 삭제하는 작업은 컴퓨터 프로그래밍의 초기 시점부터 있었으며, 특히 텍스트 처리나 데이터 클리닝 작업에 일반적으로 사용됩니다. C++ 외에도 Python, Java, Javascript 등 많은 언어들이 이 작업을 지원하는 내장 함수를 가지고 있습니다.

그밖에도 문자열의 `erase`와 `remove_if` 함수를 함께 사용하여 문자 삭제를 수행하는 등의 다른 방식도 존재합니다. 하지만 `regex_replace` 함수를 이용하면 좀 더 간편하게 그리고 강력하게 문자열 처리 작업을 할 수 있습니다.

이러한 기능들은 내부적으로 문자열을 순회하면서 각 문자를 검사하고 조건에 일치하면 해당 문자를 삭제하는 방식으로 동작합니다. 일반적으로 이러한 연산들은 O(n)의 시간 복잡도를 가집니다.

## 참고 자료

정규 표현식, 문자열 처리 등에 대한 더 많은 정보와 예제를 원하시면 아래의 링크를 참조하세요.

- C++ 정규 표현식: http://www.cplusplus.com/reference/regex/
- 문자열 처리 관련 C++ 표준 라이브러리: http://www.cplusplus.com/reference/string/
- 대체적인 방법 `remove_if` 함수에 관한 정보: http://www.cplusplus.com/reference/algorithm/remove_if/