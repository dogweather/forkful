---
title:                "C++: 컴퓨터 프로그래밍 기사 제목: 명령 줄 인수 읽기"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 왜

명령 줄 인자를 읽는 프로그램을 읽는 것에 참여하는 이유는 무엇인가요?

## 왜

커맨드 라인에서 프로그램을 실행하면 원하는 기능을 지정할 수 있습니다. 그러나 사용자가 프로그램 실행 시 추가적인 정보를 주고 싶은 경우가 있습니다. 이때 커맨드 라인에서 인자를 읽는 것이 필요합니다. 이 글에서는 C++을 통해 커맨드 라인 인자를 읽는 방법에 대해 알아보겠습니다.

## 어떻게

C++에서는 `argc`와 `argv`를 사용하여 커맨드 라인에서 인자를 읽을 수 있습니다. `argc`는 전달된 인자의 개수를 나타내고, `argv`는 인자로 전달된 문자열들을 포함하는 배열입니다. 예를 들어 다음과 같이 사용할 수 있습니다.

```C++
#include <iostream>

int main(int argc, char* argv[]) {
    for (int i = 0; i < argc; i++) {
        std::cout << "Argument " << i << ": " << argv[i] << std::endl;
    }
    
    return 0;
}
```

위의 코드는 프로그램 실행 시 전달된 인자들을 모두 출력하는 예제입니다. 예를 들어 `./program hello world`와 같이 실행하면 `argc`는 3이 되고, `argv[0]`은 프로그램 이름인 `./program`이 되고, `argv[1]`과 `argv[2]`는 `hello`와 `world`가 됩니다. 즉, `argc`와 `argv`를 이용하여 커맨드 라인에서 전달된 인자들을 사용할 수 있습니다.

## 깊이 살펴보기

C++에서 `argc`와 `argv`를 사용하는 방법은 간단하지만, 더 자세히 알아봅시다. `argc`와 `argv`는 `main()` 함수의 파라미터로 전달되는데, 이는 운영체제가 프로그램을 실행할 때 전달하는 값입니다. 또한 `argc`와 `argv`는 문자열 포인터의 배열이기 때문에 포인터 연산을 통해 인자에 접근할 수 있습니다.

또한 인자를 읽을 때 주의해야 할 점이 있습니다. 커맨드 라인에서 입력한 인자들은 모두 문자열 형태로 전달되기 때문에 다른 자료형으로 변환해야 합니다. 예를 들어, `argv[1]`은 문자열 `'hello'`가 아니라 `char*` 형태로 전달됩니다.

## 참고

- [GeeksforGeeks - C++ Command Line Arguments](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [CPP Reference - Main function](https://en.cppreference.com/w/cpp/language/main_function)
- [Tutorials Point - C++ - Command Line Arguments](https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm)

# 참고

- [GeeksforGeeks - C++ Command Line Arguments](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [CPP Reference - Main function](https://en.cppreference.com/w/cpp/language/main_function)
- [Tutorials Point - C++ - Command Line Arguments](https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm)