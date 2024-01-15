---
title:                "디버그 출력 출력하기"
html_title:           "C: 디버그 출력 출력하기"
simple_title:         "디버그 출력 출력하기"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 사용하는 이유는 단순합니다 - 코드를 디버깅하는 데 도움이 됩니다. 디버그 출력은 실행 중에 변수의 값을 확인하고 코드의 흐름을 추적하는 데 유용합니다. 이를 통해 코드를 더 빠르게 분석하고 수정할 수 있습니다.

## 방법

디버그 출력을 사용하기 위해서는 우선 코드에 `#include <stdio.h>`를 추가해야 합니다. 그리고 디버그를 원하는 위치에 `printf()` 문을 추가하면 됩니다. 예를 들어, 다음과 같이 코드를 작성할 수 있습니다.

```C
#include <stdio.h>

int main() {
    int num = 10;
    
    printf("num의 현재 값은 %d 입니다.\n", num);
    
    // 코드의 다른 부분
    
    printf("다른 부분에서 num의 값이 바뀌지는 않았습니다.\n");
    
    return 0;
}
```

위의 코드를 실행하면 다음과 같은 출력이 됩니다.

```
num의 현재 값은 10 입니다.
다른 부분에서 num의 값이 바뀌지는 않았습니다.
```

이와 같이 디버그 출력을 추가하면 변수의 값이나 코드의 이동을 추적할 수 있어 코드 분석에 매우 유용합니다.

## 깊이 파고들기

디버그 출력은 `printf()` 함수만으로 구현되지 않습니다. `stderr` 스트림을 이용하면 에러 메시지를 출력할 수 있고, `assert()` 함수를 사용하면 특정 조건이 만족되지 않을 때 프로그램을 중지시킬 수 있습니다. 또한 `#define`을 사용하여 디버그 모드를 활성화하거나 비활성화할 수도 있습니다. 이 밖에도 디버그 출력을 위해 사용할 수 있는 다양한 기능들이 있으니 참고하시기 바랍니다.

## 관련 링크

- [C 디버깅](https://en.wikipedia.org/wiki/Debugger)
- [디버그 출력을 이용한 프로그래밍 디버깅](https://www.codersbook.com/%EB%94%94%EB%B2%84%EA%B7%B8-%EC%B6%9C%EB%A0%A5%EC%9D%84-%EC%9D%B4%EC%9A%A9%ED%95%9C-%ED%94%84%EB%A1%9C%EA%B7%B8%EB%9E%A8-%EB%94%94%EB%B2%84%EA%B9%8C%EB%A6%AC-%EC%BB%B4%ED%8C%8C%EC%8A%A4)