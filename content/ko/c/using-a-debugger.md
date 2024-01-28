---
title:                "디버거 사용하기"
date:                  2024-01-26T03:47:43.605943-07:00
model:                 gpt-4-0125-preview
simple_title:         "디버거 사용하기"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디버거는 C 코드가 실행되는 동안 한 단계씩 검사하여 버그를 찾을 수 있는 도구입니다. 프로그래머는 디버거를 사용해서 코드가 어떻게 동작하는지 이해하고, 문제를 수정하며, 추측 게임 없이 성능을 최적화합니다.

## 사용 방법:
예를 들어, 숫자의 팩토리얼을 계산하는 간단한 C 프로그램을 작업 중인데 오류가 발생했다고 가정해봅시다. `gdb`(GNU Debugger)와 같은 디버거를 사용하려면 먼저 `-g` 플래그로 컴파일하여 디버그 정보를 포함시킵니다:

```c
// 컴파일: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // 음수 입력에 대한 간단한 검사
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int number = 5;
    long result = factorial(number);
    printf("%d의 팩토리얼은 %ld입니다\n", number, result);
    return 0;
}
```

그런 다음 gdb에서 실행합니다:

```shell
$ gdb ./factorial
```

`factorial` 함수에서 중단점을 설정하고 프로그램을 실행합니다:

```gdb
(gdb) break factorial
(gdb) run
```

중단점에 도달하면, `next` 또는 `n`을 사용하여 각 줄을 한 단계씩 진행하고 `print` 또는 `p`로 변수를 검사합니다:

```gdb
(gdb) next
(gdb) print 결과
$1 = 1
```

샘플 출력은 실시간 값과 프로그램 실행 흐름을 제공합니다.

## 심층 탐구
디버거는 1960년대부터 있었으며, 간단한 모니터에서 복잡한 GUI 기반 애플리케이션으로 발전했습니다. 성숙한 디버거가 개발되기 전에는 일반적으로 print-based 디버깅이 사용되었습니다. `gdb`에 대한 대안으로는 `lldb`, `dbx`, 또는 Visual Studio 또는 CLion과 같은 IDE 통합 디버거가 있습니다.

디버거를 다룰 때, 구현은 다양합니다—일부는 런타임 오류를 캐치하고, 메모리를 검사하거나, 심지어 프로그램의 실행을 역전시킬 수도 있습니다. `gdb`는 실행 중인 프로세스에 붙일 수 있어 이미 실행 중인 소프트웨어의 디버깅을 가능하게 하며, 실시간 시스템 버그 수정에 큰 도움이 됩니다.

## 참고자료
- GNU 디버거 (GDB): https://www.gnu.org/software/gdb/documentation/
- GDB를 사용한 디버깅: https://sourceware.org/gdb/current/onlinedocs/gdb
- LLDB 디버거: https://lldb.llvm.org/use/tutorial.html
- C에서의 디버깅 기법: http://www.cprogramming.com/debugging/debugging.html
