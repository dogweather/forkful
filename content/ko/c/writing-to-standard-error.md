---
title:    "C: 표준 오류에 작성하는 방법"
keywords: ["C"]
---

{{< edit_this_page >}}

## 왜

표준 에러를 쓰는 것에 참여하는 이유는 무엇일까요?

프로그래밍을 할 때, 우리는 예상치 못한 오류와 예외 상황이 발생할 수 있습니다. 이러한 상황에서 우리는 프로그램이 어떤 오류를 발생시키고 있는지 알고 싶은데, 바로 이때 표준 에러를 사용합니다. 표준 에러는 오류 메시지를 출력하는데 사용되며, 우리가 디버깅을 할 때 매우 중요한 역할을 합니다.

## 어떻게

C 프로그래밍에서 표준 에러를 쓰는 방법을 살펴보겠습니다. 먼저 `stdio.h` 헤더 파일을 포함해야 합니다. 그리고 `fprintf()` 함수를 사용하여 에러 메시지를 출력할 수 있습니다. 예를 들어, 다음과 같이 작성할 수 있습니다:

```C
#include <stdio.h>

int main() {
    int num = 0;

    if (num == 0) {
        fprintf(stderr, "오류: 0으로 나눌 수 없습니다.\n");
        return 1;
    }

    return 0;
}
```

위 코드는 `num` 변수의 값이 0일 경우에 에러 메시지를 출력하고 프로그램을 종료합니다. `stderr`는 표준 에러를 나타내는 파일 포인터입니다. 따라서 `fprintf()` 함수를 통해 `stderr`에 메시지를 출력할 수 있습니다.

## 깊이 파고들기

표준 에러를 다루는 더 깊은 정보를 살펴보겠습니다. 우선, `stderr`는 `stdout`과는 다른 파일 포인터이므로, `fprintf()` 함수를 통해 출력하는 메시지가 `stdout`에 출력되지 않습니다. 또한, `stderr`는 버퍼링되지 않으므로, 바로 출력됩니다.

여러분은 `stderr`와 `stdout`의 차이점을 잘 이해해야 합니다. 만약 `stderr`을 사용하지 않는다면, 프로그램의 실행 결과와 에러 메시지가 섞이게 됩니다. 이로 인해 디버깅이 어려워질 수 있습니다.

## 더 알아보기

- [C 표준 라이브러리: stderr](https://www.tutorialspoint.com/c_standard_library/c_function_stderr.htm)
- [C++ 문서: std::cerr](https://en.cppreference.com/w/cpp/io/cerr)
- [표준 에러를 다루는 방법](https://www.geeksforgeeks.org/error-handling-c-programs/)

## 관련 링크

- [Learn C](https://www.learn-c.org/)
- [C 언어 강좌](https://dojang.io/course/view.php?id=2)