---
title:                "문자열 보간"
html_title:           "C++: 문자열 보간"
simple_title:         "문자열 보간"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열을 삽입하는 것은 프로그래머에게 익숙한 작업일 것입니다. 이는 문자열 내부에 다른 변수나 값을 삽입하여 보다 다양한 출력을 만들어낼 수 있게 해주는 작업입니다. 프로그래머들은 이를 통해 코드를 더 유연하고 가독성 있게 만들 수 있기 때문에 이 작업을 수행합니다.

## 하는 법:
```C++
#include <iostream>

int main() {
  std::string name = "홍길동";
  int age = 30;
  
  std::cout << "내 이름은 " << name << "이고, 나이는 " << age << "살입니다." << std::endl;
  return 0;
}
```
```
// 출력:
내 이름은 홍길동이고, 나이는 30살입니다.
```

## 깊은 탐구:
### 역사적 배경:
문자열 삽입은 앞서 C언어에서는 변수와 문자열을 활용하는 방법으로 사용되었습니다. 하지만 C++언어의 등장으로 문자열에 대한 다양한 기능과 메서드가 추가되면서 문자열 삽입 역시 더욱 쉬워졌습니다.

### 대안:
문자열 삽입을 수행하는 다른 방법으로는 문자열 포매팅이 있습니다. 이는 문자열 내부에 % 기호를 사용하여 변수를 삽입하는 방법입니다. 하지만 이 방법은 포맷 문자열을 작성하는 데 있어서 번거로울 수 있고, 잘못된 포맷 문자열을 사용할 경우 오류가 발생할 수 있습니다.

### 구현 세부 사항:
C++에서 문자열 삽입은 ``<<`` 연산자를 사용하여 수행할 수 있습니다. 이를 이용하여 숫자, 문자열, 변수 등 다양한 값을 문자열 내부에 삽입할 수 있습니다. 또한 ``+`` 연산자를 사용하여 여러 개의 변수를 삽입할 수도 있습니다.

## 관련 자료:
- [C++ Reference - Basic String Operations](https://www.cplusplus.com/reference/string/basic_string/) : C++에서 문자열 다루기에 대한 기본적인 사용법과 예제를 제공하는 사이트입니다.
- [GeeksforGeeks - String Interpolation in C++](https://www.geeksforgeeks.org/string-interpolation-in-c/) : C++에서 문자열 삽입에 대한 설명과 예제를 제공하는 유용한 사이트입니다.
- [cppreference.com - String Interpolation](https://en.cppreference.com/w/cpp/language/types#String_literals) : C++에서 문자열 삽입에 대한 공식 문서를 확인할 수 있는 사이트입니다.