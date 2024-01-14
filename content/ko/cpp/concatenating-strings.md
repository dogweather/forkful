---
title:                "C++: 스트링 연결하기"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

# 왜: 
문자열을 연결하는 것은 C++ 프로그래밍에서 일반적이고 유용한 기술입니다. 이를 통해 여러 문자열을 하나의 문자열로 결합할 수 있어 코드를 더 깔끔하고 읽기 쉽게 만들 수 있습니다.

## 어떻게: 
문자열 연결은 간단한 작업입니다. 먼저 ```+``` 연산자를 사용하여 두 문자열을 함께 더하면 됩니다. 예를 들어, 다음과 같은 코드를 작성할 수 있습니다. 

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str1 = "Hello";
    string str2 = "World";
    string result = str1 + " " + str2; // str1과 str2를 공백으로 구분하여 연결
    cout << result << endl; // "Hello World" 출력

    return 0;
}
```

위 코드에서 사용된 ```+``` 연산자는 문자열뿐만 아니라 모든 기본 자료형에도 사용할 수 있습니다. 따라서 문자열과 다른 자료형을 함께 연결하는 것도 가능합니다. 또한, 여러 개의 문자열을 연결할 때에도 ```+``` 연산자를 이용하여 하나의 큰 문자열로 결합할 수 있습니다.

## 깊은 이해: 
문자열 연결은 내부적으로 문자열을 조작하는 메모리 할당 및 복사 작업이 발생하므로, 불필요한 문자열 연결은 프로그램의 성능을 저하시킬 수 있습니다. 따라서, 매우 큰 반복 작업이나 빈번한 문자열 연결이 필요한 경우에는 대안적인 방법을 고려하는 것이 좋습니다.

# 연관 자료:
- [C++ 문자열 클래스 및 연산자 설명서](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [C++ 문자열 연결 예제 코드](https://www.geeksforgeeks.org/cpp-program-for-concatenating-two-strings/)
- [C++ 성능 최적화 관련 블로그](https://www.codeproject.com/Articles/318139/Concatenating-Strings-and-Performance-Impact)