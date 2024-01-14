---
title:                "C++: 디버그 출력하기"
simple_title:         "디버그 출력하기"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜
확률적으로 C++ 프로그래머들은 디버깅 단계에서 간단한 변수 값들을 콘솔에 출력할 필요성을 느낍니다. 이를 통해 코드 실행 상황을 더 자세히 파악하고 문제 해결을 더 쉽게 할 수 있습니다.

## 하우 투 (양식)
```C++
#include <iostream>

using namespace std;

int main() {
    int age = 26;
    string name = "Jane";
    
    // 디버그 출력
    cout << name << "의 나이는 " << age << "살입니다." << endl;
    
    return 0;
}

// 콘솔 출력
Jane의 나이는 26살입니다. 
```

## 딥 다이브
디버깅 출력문은 간단하지만 매우 유용한 기능입니다. 따라서 가능한 한 많은 정보를 출력하는 것이 좋습니다. 예를 들어, 루프나 조건문 안에서 변수의 값 변화를 출력하면 코드의 실행 흐름을 더 잘 이해할 수 있습니다. 또한, 디버그 출력문을 활용하여 어떤 함수나 객체의 동작을 추적할 수도 있습니다.

## 이것도 볼래요
- [디버그 출력문의 활용 방법](https://www.geeksforgeeks.org/debugging-in-c/)
- [C++ 디버그 출력문 사용하기](https://www.tutorialspoint.com/cplusplus/cpp_debugging.htm)
- [디버깅 출력문을 활용하여 코드 디버깅하기](https://www.learncpp.com/cpp-tutorial/using-debugging-output/)