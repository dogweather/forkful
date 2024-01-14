---
title:                "C++: 문자열을 소문자로 변환하기"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 왜:

스트링을 소문자로 변환하는 것에 참여해야 하는 이유는 다양합니다. 예를 들어, 사용자의 입력을 받아서 대소문자를 구분하지 않는 검색이나 비교를 해야 하는 경우가 있을 수 있습니다. 또는 문자열에서 소문자로만 이루어진 새로운 문자열을 만들어야 할 때가 있을 수도 있습니다. 즉, 소문자로 변환하는 것은 문자열 데이터를 더 편리하게 다룰 수 있도록 도와주는 중요한 기술입니다.

# 방법:

다음은 C++로 문자열을 소문자로 변환하는 방법을 보여주는 간단한 예시 코드입니다. 

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
    // 대문자와 소문자가 혼합된 문자열
    string str = "Hello World";

    // 문자열을 소문자로 변환
    for (int i = 0; i < str.length(); i++) {
        str[i] = tolower(str[i]);
    }

    cout << str << endl; // 출력 결과: hello world

    return 0;
}
```

이처럼, `tolower()` 함수를 사용하면 쉽게 문자열을 소문자로 변환할 수 있습니다.

# 딥 다이브:

문자열을 소문자로 변환하는 방법에 대해 좀 더 깊이 살펴보겠습니다. C++의 `std::tolower()` 함수는 `cctype` 헤더 파일에 정의되어 있습니다. 이 함수는 `int` 형식의 문자를 매개변수로 받아 해당 문자의 소문자를 리턴해주는 역할을 합니다. 이때, 매개변수의 형식은 `int`이지만 해당 값은 ASCII 코드로 표현된 문자의 정수 값이어야 합니다.

그리고 `std::string` 클래스는 문자열 데이터를 다루는 많은 함수와 연산자를 제공하는데, 이 중에서 `operator[]` 연산자(대괄호로 문자열의 특정 인덱스에 접근하는 연산)는 문자열의 특정 위치에 있는 문자에 대한 참조를 리턴합니다. 따라서 `std::string` 변수의 값을 변경하려면 이 연산자와 같이 사용하여 문자열의 특정 위치에 있는 문자에 접근해야 합니다.

# 참고 자료:

- [C++ string class](https://www.geeksforgeeks.org/stdstring-class-in-c/)
- [std::tolower() 함수](https://www.cplusplus.com/reference/cctype/tolower/)
- [operator[] 연산자](https://www.geeksforgeeks.org/operator-in-c-cpp-with-examples/)