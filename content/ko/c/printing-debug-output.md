---
title:                "디버그 출력을 찍어보기"
date:                  2024-01-20T17:52:41.475564-07:00
model:                 gpt-4-1106-preview
simple_title:         "디버그 출력을 찍어보기"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
'디버그 출력'이란 코드가 어떻게 실행되고 있는지 이해하기 위해 메시지를 출력하는 것입니다. 프로그래머들은 오류를 찾고 코드의 동작을 확인하기 위해 디버그 출력을 사용합니다.

## How to (어떻게 하나요?)
C 코드에서 디버그 정보를 출력하는 가장 단순한 방법은 `printf` 함수를 사용하는 것입니다. 근데 실제 개발할 때는 조금 더 진화한 방법을 쓰게 됩니다. 예를 들어보죠:

```c
#include <stdio.h>

int main() {
    int variable = 42;
    printf("Debug Output: %d\n", variable); // 단순 출력

    #ifdef DEBUG
    fprintf(stderr, "Debug: variable is %d\n", variable); // 조건부 디버그 출력
    #endif

    // ... 코딩 계속 ...

    return 0;
}
```

실행 결과는 단순한 텍스트 메시지입니다.
```
Debug Output: 42
```
`DEBUG`가 정의됐다면,
```
Debug: variable is 42
```

## Deep Dive (심층 분석)
옛날엔 디버그 정보가 종이에 찍혀 나왔습니다. 이제는 개발 툴과 IDE가 이를 대체했습니다. `printf`는 여전히 유용하지만, `assert`나 로거 라이브러리와 같은 방법들도 있습니다. 이런 도구들을 사용하면 출력을 조건부로 관리할 수 있고, 정보 레벨을 설정할 수 있습니다. 예를 들어, `assert`는 개발 중에만 활성화되고, 로거는 경고, 에러, 디버그 메시지를 분류할 수 있게 합니다.

`fprintf`를 `stderr`에 사용하면, 표준 오류 스트림으로 메시지를 출력하는데, 이는 다른 출력(표준 출력에 나타나는 `printf` 메시지)과 분리될 수 있기 때문에 유용합니다. `stderr`는 보통 콘솔이나 터미널에 바로 출력되므로 로그 파일에 기록하지 않아도 바로 볼 수 있습니다.

조건부 컴파일은 `#ifdef DEBUG` 같은 전처리기 지시문을 사용해 특정 코드가 디버그 빌드에서만 실행되도록 할 수 있습니다. 이는 프로덕션 코드에 불필요한 디버그 정보가 포함되지 않도록 해줍니다.

## See Also (참고 자료)
- C 프로그래밍 튜토리얼: https://www.learn-c.org/
- `printf` 매뉴얼 페이지: https://en.cppreference.com/w/c/io/fprintf
- GNU 디버거(GDB) 사용하기: https://www.gnu.org/software/gdb/documentation/
- 로깅을 위한 syslog 함수: https://man7.org/linux/man-pages/man3/syslog.3.html
- 조건부 컴파일에 대한 설명: https://en.wikipedia.org/wiki/C_preprocessor#Conditional_compilation

가끔은 디버그 출력만큼 간단하고 빠른 해결책이 없으니 깔끔하게 사용하시면 됩니다. Happy coding!
