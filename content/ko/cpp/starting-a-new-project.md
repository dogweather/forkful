---
title:                "새 프로젝트 시작하기"
html_title:           "Arduino: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "C++"
category:             "C++"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇인지 & 왜 그렇게 하는건지?

새 프로젝트를 시작하다는 것은 아무래도 새로운 아이디어를 실현하기 위한 코드를 작성하기 시작하는 것을 말합니다. 이를 위해 프로그래머들은 초기 구상 및 설계단계를 거쳐 코드를 구현하게 됩니다.

## 어떻게 할까요:

다음은 간단한 C++ 프로젝트를 시작하는 방법입니다.

```C++
// include the necessary libraries
#include <iostream>

// main function - this is where the actual coding magic happens
int main() {
    std::cout << "새로운 프로젝트를 시작해볼까요? 화이팅!\n";
    return 0;  //this is used to indicate that the program has ended without error.
}
```

이 코드를 실행해보면, 터미널에 다음과 같이 출력될 것입니다:

```
새로운 프로젝트를 시작해볼까요? 화이팅!
```

## 딥다이브

**역사적 배경**: C++는 1979년에 최초로 개발된 언어입니다. 그 이후로 C++는 발전하면서 시간이 지날수록 효율적인 코딩을 가능하게 하는 다양한 기능들이 추가되었습니다.

**대안**: Python, Java, JavaScript 등은 C++에 대한 인기 있는 대안입니다. 단, 프로젝트 상황이나 요구사항에 따라 가장 적합한 언어를 선택해야 합니다.

**구현 상세**: 새 프로젝트를 시작할 때 초기 단계에서 구조와 설계를 명확히 하는 것이 중요합니다. 좋은 설계는 효율적인 코드를 작성하는데 중요한 역할을 합니다.

## 참고하기 

1. [시작하기: C++ 시작하기](https://www.learn-cpp.org/)
2. [중급자: C++의 중급 개발자를 위한 가이드](https://www.advancedcplusplus.com/)
3. [전문가: C++의 고급 사용자를 위한 리소스](https://en.cppreference.com/)