---
title:                "C: 디버그 출력 출력하기"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 프로그래밍하는 것에 관심이 있을까요? 디버그 출력은 또 다른 도구를 사용하기 전에 시스템 또는 코드를 검토하는 간단하고 효과적인 방법입니다. 또한 코드 에러를 신속하게 해결하는 데 도움이 됩니다.

## 어떻게

C 프로그래밍에서 디버그 출력을 활용하는 방법을 알아보겠습니다. 아래 코드 블록은 디버그 출력을 위한 기본적인 예제입니다.

```C
#include <stdio.h>

int main(void) {

    int x = 5;
    int y = 10;
    int z = x * y;
    
    // 디버그 출력
    printf("x 값: %d \n", x);
    printf("y 값: %d \n", y);
    printf("곱셈 결과: %d \n", z);

    return 0;
}

/* 출력 예시:
x 값: 5 
y 값: 10 
곱셈 결과: 50 
*/
```

위 예제에서 정의한 변수들과 계산 결과를 디버그 출력을 통해 확인할 수 있습니다. 이렇게 간단한 방법으로 코드를 검토하고 문제를 해결할 수 있습니다.

## 깊이 들어가기

디버그 출력을 더욱더 효율적으로 활용하기 위해서는 다양한 방법을 익혀야 합니다. 예를 들어, `printf` 함수 대신 디버그 라이브러리를 사용하거나, 디버그 출력을 파일에 저장하거나, 조건부 디버그 출력을 설정하는 등의 방법이 있습니다. 디버깅에 대한 좀 더 자세한 내용은 다음 링크를 참고하세요.

## 관련 링크

- [디버그 출력을 활용한 디버깅 방법](https://www.ics.uci.edu/~cs23/DEPTHS/EXAMPLES/DEBUG/)
- [디버그 라이브러리 설명서](https://www.gnu.org/software/libc/manual/html_node/Debugging-Output.html)
- [조건부 디버그 출력 설정 방법](https://stackoverflow.com/questions/142508/how-do-i-turn-off-conditional-debugging-with-print-statements)