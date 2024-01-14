---
title:    "C: 디버그 출력 출력"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/printing-debug-output.md"
---

{{< edit_this_page >}}

# 왜 디버그 출력을 사용해야 할까요?

디버그 출력은 프로그램을 디버깅하는데 매우 유용합니다. 프로그램이 실행될 때 변수의 값이나 프로그램의 흐름을 확인할 수 있어서 버그를 발견하고 수정하는데 도움이 됩니다.

## 디버그 출력하는 방법

디버그 출력은 ```printf()``` 함수를 사용하여 쉽게 할 수 있습니다. 아래 예제를 참고하세요.

```C
#include <stdio.h>

int main() {
  // 변수에 값 할당
  int num1 = 10;
  int num2 = 20;

  // 디버그 출력
  printf("num1의 값: %d\n", num1);
  printf("num2의 값: %d\n", num2);

  return 0;
}
```

위의 코드를 실행하면 다음과 같은 출력이 나옵니다.

```
num1의 값: 10
num2의 값: 20
```

위의 예제처럼 ```printf()``` 함수를 사용하여 변수의 값을 확인할 수 있습니다.

## 디버그 출력 깊게 살펴보기

디버그 출력을 세밀하게 관리하려면 ```#define``` 전처리기를 사용할 수 있습니다. 아래 예제를 참고하세요.

```C
#include <stdio.h>

// DEBUG 모드 활성화
#define DEBUG 1

int main() {
  // 변수에 값 할당
  int num1 = 10;
  int num2 = 20;

  // 디버그 출력
  #ifdef DEBUG
    printf("num1의 값: %d\n", num1);
    printf("num2의 값: %d\n", num2);
  #endif

  return 0;
}
```

위의 코드에서 DEBUG 모드를 활성화하면 디버그 출력이 실행되지만 비활성화하면 디버그 출력이 실행되지 않습니다.

# 더 알아보기

이 블로그 포스트에서는 디버그 출력이 무엇이고 어떻게 사용하는지에 대해 알아보았습니다. 더 자세한 내용은 아래 링크를 참고하세요.

- https://www.tutorialspoint.com/cprogramming/c_debugging.htm
- https://www.guru99.com/c-programming-debugging.html

# 관련 링크

- https://ko.wikipedia.org/wiki/C_언어
- https://www.tutorialspoint.com/cprogramming/index.htm