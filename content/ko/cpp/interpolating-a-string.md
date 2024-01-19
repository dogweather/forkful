---
title:                "문자열 보간하기"
html_title:           "Java: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 사용하는가?

문자열 내삽(Interpolating a string)은 변수 또는 표현식을 쉽게 문자열에 포함시키는 기법입니다. 이는 코드의 가독성을 높이고 문법적인 오류를 최소화하기 위해 프로그래머들이 자주 사용합니다.

## 어떻게 사용하는가:

C++에서 문자열 내삽은 아래와 같이 간단하게 이루어질 수 있습니다.

```C++
#include <iostream>
#include <string>

int main() {
    std::string name = "John";
    int age = 20;
    
    std::cout << "Hello, my name is " << name << " and I am " << age << " years old.\n";
    
    return 0;
}
```

코드를 실행하면 출력되는 결과는 다음과 같습니다:

```C++
Hello, my name is John and I am 20 years old.
```

## 깊은 탐구:

1. 과거의 C++에서는 문자열 내삽을 하는 방법이 제한적이었으며, 대부분의 경우 문자열 연결(string concatenation) 또는 sprintf 함수를 사용해야 했습니다. 

2. 하지만 현재의 C++에서는 더 효율적인 방법들이 제공됩니다. 그 중 하나가 위에서 보여준 `<<` 연산자를 이용하는 방법입니다.

3. C++에서 문자열 내삽을 구현하는 또 다른 방법은 `std::ostringstream` 라이브러리를 사용하는 것입니다. 이 방법을 사용하면 변수를 포함한 문자열을 완벽하게 제어할 수 있습니다.

## 참고자료:

다음은 문자열 내삽에 관련된 추가 정보와 자료를 확인 할 수 있는 사이트 및 문서입니다:

1. [C++ Strings - w3schools](https://www.w3schools.com/cpp/cpp_strings.asp)
2. [C++ String Interpolation - stackoverflow](https://stackoverflow.com/questions/2342162/stdstring-formatting-like-sprintf)
3. [C++ String Manipulation - cppreference](https://en.cppreference.com/w/cpp/string)
4. [C++ User-Defined Literals - cplusplus.com](http://www.cplusplus.com/doc/tutorial/constants/)