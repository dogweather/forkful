---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 사용하는가? 
문자열을 소문자로 변환하는 것은 대문자가 있는 문자열에서 모든 대문자를 대응하는 소문자로 바꾸는 것을 의미합니다. 프로그래머들이 이를 사용하는 주된 이유는 대소문자 구분 없이 데이터를 일관성 있게 처리하고, 사용자 입력을 정규화하기 위해서입니다.

## 어떻게 사용하나? 
C++에서는 표준 `<algorithm>` 라이브러리의 `transform()` 함수를 이용해 문자열을 소문자로 변환할 수 있습니다.

```C++
#include <algorithm>
#include <cctype>
#include <string>

int main() {
    std::string str = "Hello, World!";
    std::transform(str.begin(), str.end(), str.begin(), ::tolower);
    std::cout << str;
    return 0;
}
```

이는 "hello, world!"라는 결과를 출력하게 됩니다.

## 깊은 이해 
C++에서 문자열을 소문자로 변환하는 방법은 오래 전부터 사용되었습니다. 이는 `<algorithm>` 표준 라이브러리와 `tolower()` 함수의 조합으로 구현되었습니다. 

다른 가능한 방법으로는 C++17부터 표준 라이브러리에 추가된 `std::tolower`를 사용하는 것이 있습니다. 이는 직접적으로 문자열에 적용할 수 없으므로, 문자열의 각 문자에 대해 호출해야 하는 한계가 있습니다. 

`transform()` 함수가 작동하는 방식은 문자열의 시작과 끝을 정의하고, 해당 범위의 각 요소에 대해 `tolower` 함수를 적용함으로써 동작합니다. 이는 원본 문자열을 변경하며, 대문자가 없는 문자열에 `transform()`을 사용하더라도 아무런 영향을 미치지 않습니다.

## 참고자료 
1. [cplusplus.com: transform()](http://www.cplusplus.com/reference/algorithm/transform/)
2. [cplusplus.com: tolower()](http://www.cplusplus.com/reference/cctype/tolower/)
3. [StackOverflow: How to convert a string to lower case in C++?](https://stackoverflow.com/questions/313970/how-to-convert-a-string-to-lower-case-in-c)