---
title:                "표준 에러에 쓰는 방법"
html_title:           "C: 표준 에러에 쓰는 방법"
simple_title:         "표준 에러에 쓰는 방법"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

표준 오류에 쓰기에 참여할 이유를 짧게 설명합니다. 

때로는 디버깅을 위해 프로그램에서 발생하는 오류 메시지를 표준 오류에 출력하는 것이 매우 유용할 수 있습니다. 이를 통해 사람들은 프로그램이 어디서 문제를 일으키고 있는지에 대한 정보를 얻을 수 있습니다. 또한 표준 오류에 메시지를 출력하면 로그 파일을 생성하는 것보다 간단하며 디버깅 과정이 더욱 편리해집니다.

## 어떻게

아래의 예제 코드와 샘플 출력을 살펴보면서 표준 오류에 쓰는 법을 익혀보세요.

```C
#include <stdio.h>

int main() {
    int x = 2;
    int y = 0;
    float result;

    if (y == 0) {
        fprintf(stderr, "Cannot divide by zero!");
    }
    else {
        result = (float)x / y;
        printf("%d divided by %d is %f", x, y, result);
    }
    return 0;
}
```

위의 예제 코드에서는 먼저 `stderr` 파일을 `fprintf()` 함수를 사용하여 열고 메시지를 출력합니다. `stderr`는 표준 오류 파일로서 주로 에러 메시지를 출력하는 데 사용됩니다. 그리고 `printf()` 함수를 사용하여 정상적인 출력을 `stdout` 파일에 하도록 합니다. 이렇게 함으로써 사용자는 오류 메시지와 함께 프로그램의 다른 부분의 출력도 동시에 볼 수 있습니다.

## 딥 다이브

보통 프로그램에서 오류 메시지를 출력할 때 `fprintf(stderr, ...)`와 같은 방식을 사용합니다. 하지만 여러분은 `perror()`와 같은 더 편리한 함수도 사용할 수 있습니다. `perror()` 함수를 사용하면 오류 메시지와 함께 해당 에러 코드의 의미를 알려줍니다. 예를 들어, `perror("Cannot open file")`를 호출하면 에러 메시지 뒤에 해당 오류 코드의 뜻인 "No such file or directory"가 출력됩니다.

또한 `errno`이라는 전역 변수를 사용하여 발생한 오류의 코드를 얻을 수 있습니다. `errno`이 0이 아닌 값을 가지고 있다면 어떤 오류가 발생했는지 확인할 수 있습니다.

위에서 소개한 방법들을 사용하면 더욱 편리하게 표준 오류에 쓸 수 있습니다. 하지만 주의할 점은 `stderr` 출력이 크게 성능에 영향을 주지는 않지만, 가능하면 남용하지 않는 것이 좋습니다.

## 참고

- [C 표준 라이브러리 문서](https://www.gnu.org/software/libc/manual/html_node/Error-Reporting.html)
- [C 프로그래밍 입문서](https://ko.wikipedia.org/wiki/C_%ED%94%84%EB%A1%9C%EA%B7%B8%EB%9E%A8)