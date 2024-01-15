---
title:                "문자열 연결하기"
html_title:           "C++: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 이어붙이기(concatenate)는 프로그래밍에서 자주 사용되는 기술입니다. 이를 통해 여러 개의 문자열을 하나로 합치고, 보다 복잡한 문제를 해결할 수 있습니다. 따라서 프로그래밍에 많이 관심이 있는 분이라면 문자열 이어붙이기를 배우는 것이 중요합니다.

## 어떻게 하나요?

먼저, 문자열을 이어붙이는 간단한 방법은 + 연산자를 사용하는 것입니다. 예를 들어서 다음과 같이 코드를 작성할 수 있습니다.
```C++
#include <iostream>
#include <string> //string 헤더파일 포함

int main()
{
    std::string name = "John";
    std::string greeting = "Hello, " + name + "!";
    
    std::cout << greeting << std::endl; // Hello, John!
    
    return 0;
}
```

또 다른 방법으로는 `append()` 함수를 사용하는 것입니다. 이 함수는 문자열을 추가할 수 있도록 스트링 객체의 끝에 다른 문자열을 이어 붙여줍니다. 예를 들어서 다음과 같이 사용할 수 있습니다.

```C++
#include <iostream>
#include <string> //string 헤더파일 포함

int main()
{
    std::string message = "Hello, ";
    message.append("John").append("!");
    
    std::cout << message << std::endl; // Hello, John!
    
    return 0;
}
```
출력 결과는 위와 동일합니다.

## 딥 다이브

문자열을 이어붙이기는 C++에서 매우 중요한 개념입니다. 이를 통해 코드의 가독성을 높일 수 있고, 보다 복잡한 문자열 처리를 할 수 있게 됩니다. 또한, 사용되는 함수와 연산자에 대한 이해도가 높으면 효율적으로 문자열 이어붙이기를 할 수 있습니다.

특히 `string` 헤더파일에서 제공하는 `append()` 함수를 사용한다면 더 효과적인 문자열 이어붙이기가 가능합니다. 이 함수는 여러 개의 문자열을 하나의 문자열로 합칠 수 있도록 유용한 기능을 제공합니다.

## See Also

- [C++ string concatenation](https://www.geeksforgeeks.org/concatenate-two-strings-in-cpp/)
- [C++ strings and character pointers](https://www.programiz.com/cpp-programming/string-character-pointers)
- [C++ string append function](https://www.tutorialspoint.com/cpp_standard_library/cpp_string_append.htm)