---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇과 왜?


문자열 결합이란 하나 이상의 문자열을 하나의 문자열로 결합하는 것입니다. 프로그래머들은 데이터를 더 효율적으로 처리하고 시각적으로 출력하기 위해 문자열을 결합합니다.

## 어떻게 수행합니까?

아래의 C++ 코드 예제는 문자열 결합을 보여줍니다.

```C++
#include <iostream>
#include <string>

int main() {
    std::string str1 = "Hello";
    std::string str2 = " World!";
    std::string str3 = str1 + str2;
    std::cout << str3;

    return 0;
}
```

이 프로그램을 실행하면 `Hello World!`가 출력됩니다.

## 깊은 이해

문자열 결합은 C++에서 자주 사용되는 기능으로, 모든 데이터 유형과 연계하여 사용할 수 있습니다. 이를 처음 도입한 1970년대 C언어에서 원래 사용자는 `strcat`함수를 사용하여 문자열을 결합하였으나, 이는 보안 문제로 문제로 인식되어 왔습니다.

C++에서는 더 개선된 `std::string` 클래스와 `+` 연산자를 사용해 안전하고 효율적으로 문자열을 결합할 수 있습니다. 핵심은 과도한 메모리 할당을 피하는 것입니다.

이외에도, `std::stringstream` 또는 `std::string::append`등의 방법을 통해 문자열을 결합할 수 있습니다.

## 참고 자료

- [String library in C++](https://www.cplusplus.com/reference/string/)
- [Stack Overflow: Concatenating strings in C++](https://stackoverflow.com/questions/18892281/most-logical-way-to-concatenate-strings)
- [C++ Standards Committee Paper: std::string Concatenation and Operator ‘+’ vs. ‘+=’](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0980r0.html)