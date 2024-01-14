---
title:                "C++: 스트링 대문자로 바꾸기"
simple_title:         "스트링 대문자로 바꾸기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 첫 글자를 대문자로 변환하는 것의 이유는 일반적으로 텍스트를 정규화하거나 제목을 형식을 맞추기 위해서입니다. 또한, 프로그래밍에서 특정한 문자열을 원하는 형식으로 변경할 때 유용합니다.

## 어떻게

```C++
#include <iostream>
#include <string>

using namespace std;

// 문자열의 첫 글자를 대문자로 변환하는 함수
string capitalize(string str) {
  // 문자열의 첫 번째 글자를 대문자로 변환
  str[0] = toupper(str[0]);
  return str;
}

int main() {
  // 원본 문자열
  string str = "hello world";

  cout << "원본: " << str << endl;
  
  // 변환 함수를 사용하여 첫 글자를 대문자로 변환
  str = capitalize(str);
  
  cout << "변환: " << str << endl;

  return 0;
}
```

```
출력:
원본: hello world
변환: Hello world
```

## 깊게 들어가기

문자열을 처리하는 방법은 프로그래밍 언어에 따라 다를 수 있지만, 일반적으로 문자열은 많은 특징과 함수를 가지고 있습니다. 예를 들어, C++에서는 적용할 수 있는 다양한 문자열 함수들이 존재합니다. `toupper()` 함수를 사용하여 문자를 대문자로 변환하는 것이 그 중 하나입니다. 또한, `string` 라이브러리에서 제공하는 `toupper()` 함수 이외에도 `ctype.h` 라이브러리에서 제공하는 `toupper()` 함수를 사용할 수도 있습니다.

## 더 알아보기

- [C++ string 라이브러리](https://www.cplusplus.com/reference/string/)
- [ctype.h 라이브러리](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)

## 관련 링크

- [C++ 프로그래밍 기초: 문자열](https://wikidocs.net/1393)