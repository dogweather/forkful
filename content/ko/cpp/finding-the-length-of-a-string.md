---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열의 길이를 찾는 것이란 문자열에 포함된 문자의 수를 계산하는 것입니다. 프로그래머들이 이 작업을 수행하는 이유는 주로 입력 데이터의 유효성 검사, 반복, 부분 문자열 연산 등을 수행하기 위함입니다.

## 어떻게 하는가:

C++에서 문자열의 길이를 찾는 가장 흔한 방법은 표준 라이브러리에 포함된 `length()` 함수를 사용하는 것입니다.

```C++
#include<iostream>
#include<string>

int main() {
   std::string str = "강남역";
   std::cout << "Length of string is: " << str.length();
   return 0;
}
```

위 프로그램을 실행하면 다음과 같은 출력이 생깁니다:
```
Length of string is: 3
```

## 깊이있게 알아보기

문자열의 길이를 찾는 몇 가지 대안 방법이 존재합니다. 예를 들어, C 스타일 문자열에서는 null 종료 문자를 새는 `strlen()` 함수를 사용할 수 있습니다. 하지만 C++에서는 표준 라이브러리에 내장된 `length()`나 `size()` 함수를 사용하는 것이 더 안전하며 권장되는 방법입니다.

`length()` 함수는 문자열에 저장된 문자의 수를 반환하며, 이 함수는 문자열의 끝을 나타내는 null 문자를 계산에 포함하지 않습니다. `length()` 함수는 O(1) 시간 복잡도를 가지므로 문자열의 길이를 찾는 데 매우 빠릅니다.

## 관련 자료

- [C++ String length()](http://www.cplusplus.com/reference/string/string/length/)
- [C++ String size()](http://www.cplusplus.com/reference/string/string/size/)
- [C++ strlen()](http://www.cplusplus.com/reference/cstring/strlen/)