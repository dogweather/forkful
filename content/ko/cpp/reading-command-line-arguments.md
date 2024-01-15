---
title:                "컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
html_title:           "C++: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

C++ 프로그램에서 커맨드 라인 인수를 읽는 것은 매우 중요합니다. 이를 통해 사용자는 프로그램을 실행할 때 옵션과 인수를 제공하고 원하는 결과를 얻을 수 있습니다. 따라서 이 기능을 습득하는 것은 C++ 프로그래밍에서 필수적입니다.

## 어떻게

커맨드 라인 인수를 읽는 방법은 간단합니다. 먼저 `main()` 함수의 매개변수 `argc`와 `argv`를 사용하여 커맨드 라인 인수의 개수와 값을 받아옵니다. 이후 `for` 반복문을 사용하여 `argv` 배열에서 값을 순서대로 읽어오면 됩니다.

```C++
#include <iostream>

int main(int argc, char* argv[]) {
    // 프로그램의 매개변수 개수 출력
    std::cout << "커맨드 라인 인수 개수: " << argc << std::endl;
    // 모든 인수를 출력
    std::cout << "커맨드 라인 인수: ";
    for (int i = 0; i < argc; i++) {
        std::cout << argv[i];
        if (i < argc - 1) {
            std::cout << ", ";
        }
    }
    // 실행 중지
    return 0;
}
```

**출력:**

```
커맨드 라인 인수 개수: 4
커맨드 라인 인수: ./program, -o, output.txt, -v
```

## 깊게 더 들어가보기

`argc`와 `argv`가 어떻게 동작하는지 알아보기 위해 실행파일을 터미널에서 다른 인수와 함께 실행해보면 `argv` 배열이 어떻게 변하는지 확인할 수 있습니다. 예를 들어 다음과 같이 실행했을 때

```
./program -n --help output.txt -v
```

`argv` 배열은 다음과 같이 변합니다.

```C++
argv[0]: ./program
argv[1]: -n
argv[2]: --help
argv[3]: output.txt
argv[4]: -v
```

즉, `argv[0]`에는 실행파일의 경로가, `argv[1]`부터는 옵션과 인수가 순서대로 들어오게 됩니다. 이를 활용하여 `argc`와 `argv`를 조합해서 사용자가 원하는 대로 프로그램을 실행할 수 있습니다.

## 관련 링크

- [C++ - 커맨드 라인 인수](https://sjh836.tistory.com/141)
- [C++ - 포인터 배열로 커맨드 라인 인수 다루기](http://www.tcpschool.com/cpp/cpp_stream_argc)
- [C++ - 커맨드 라인 인수를 이용한 간단한 예제 코드](https://modoocode.com/271)