---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇이며 왜합니까?

문자열에서 부분 문자열을 추출하는 것은 문자열의 특정 부분을 조작하고자 할 때 수행하는 작업입니다. 프로그래머가 이를 수행하는 이유는 데이터 처리 및 분석을 위해 문자열을 다양한 조각으로 분할하는 것입니다.

## 어떻게 하나요:

C++에서는 `substr` 함수를 이용하여 문자열에서 부분 문자열을 추출할 수 있습니다.
```C++
#include <string>
#include <iostream>

int main()
{
    std::string s = "C++ Programming Language";
    std::string part = s.substr(0, 3);

    std::cout << part;
    return 0;
}
```
출력:
```C++
C++
```
위의 예에서 `0`은 시작 인덱스를 나타내며 `3`은 추출할 문자 수입니다.

## 깊게 들어가기:

`substr` 메서드는 C++ Standard Library의 일부로 1998년에 첫 C++ 표준인 C++98에서 소개되었습니다. 이전에는 C 스타일 문자열과 같은 더 원시적인 방식으로 문자열을 조작해야 했습니다.

대신에 `find`와 `erase`와 같은 다른 문자열 메소드를 사용하여 부분 문자열을 추출 할 수도 있습니다. 하지만 이는 좀더 복잡합니다.

성능 측면에서, `substr` 메서드는 문자열의 길이(즉, 추출할 문자열의 길이)에 비례하는 시간 복잡도를 갖습니다.

## 참고 자료:

아래 링크들은 이 주제와 관련된 추가 정보를 제공합니다.

1. [C++ string substr](http://www.cplusplus.com/reference/string/string/substr/)
2. [C++ string find](http://www.cplusplus.com/reference/string/string/find/)
3. [C++ string erase](http://www.cplusplus.com/reference/string/string/erase/)