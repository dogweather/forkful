---
title:                "C++: 표준 에러에 쓰는 것"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜 

표준 에러에 쓰기에 참여하는 이유는 소프트웨어 개발에서 중요한 역할을 합니다. 프로그램에서 에러를 파악하고 수정하는데 도움이 됩니다.

## 방법 

이제 C++의 표준 에러에 쓰는 방법을 알아보겠습니다. 아래 코드 블록에서 ```std::cerr```를 사용하여 표준 에러에 문자열을 보낼 수 있습니다.

```C++
#include <iostream>

int main() {
    std::cerr << "표준 에러에 쓰기!" << std::endl;
    return 0;
}
```

위 코드를 실행하면 다음과 같은 출력을 볼 수 있습니다.

```
표준 에러에 쓰기!
```

이제 위 코드를 조금 더 복잡한 방식으로 수정해보겠습니다. 아래 코드는 사용자로부터 입력받은 문자열을 표준 에러에 출력하는 예시입니다.

```C++
#include <iostream>

int main() {
    std::string str;
    std::cerr << "문자열을 입력하세요: ";
    std::cin >> str;
    std::cerr << "입력받은 문자열: " << str << std::endl;
    return 0;
}
```

위 코드를 실행하면 다음과 같은 출력을 볼 수 있습니다.

```
문자열을 입력하세요: Hello World!
입력받은 문자열: Hello
```

## 깊이 알아보기 

표준 에러에 쓰는 것은 프로그램의 디버깅 과정에서 매우 중요합니다. 에러가 발생한 위치와 관련 정보를 표준 에러에 쓰면, 디버그하기 쉽고 효율적으로 문제를 해결할 수 있습니다. 또한, 표준 에러에 쓰는 것은 파일 입출력에서 중요한 역할을 합니다. 에러가 발생했을 때, 이를 파일로 저장하면 추후 분석이 가능합니다.

## 참고 자료 

- [C++ 표준 에러에 쓰기 (cppreference.com)](https://en.cppreference.com/w/cpp/io/cerr)
- [C++ 에러 처리 (cplusplus.com)](http://www.cplusplus.com/doc/tutorial/errors/)
- [C++ 입출력 (cplusplus.com)](http://www.cplusplus.com/doc/tutorial/files/)