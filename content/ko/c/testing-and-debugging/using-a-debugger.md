---
aliases:
- /ko/c/using-a-debugger/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:12.710924-07:00
description: "C\uC5D0\uC11C\uC758 \uB514\uBC84\uAC70\uB294 \uAC1C\uBC1C\uC790\uAC00\
  \ \uCF54\uB4DC\uB97C \uB2E8\uACC4\uBCC4\uB85C \uC2E4\uD589\uD558\uACE0, \uBCC0\uC218\
  \uB97C \uAC80\uC0AC\uD558\uACE0, \uC2E4\uD589 \uD750\uB984\uC744 \uBAA8\uB2C8\uD130\
  \uB9C1\uD560 \uC218 \uC788\uB3C4\uB85D \uD558\uB294 \uC804\uBB38 \uB3C4\uAD6C\uC785\
  \uB2C8\uB2E4. \uC774 \uACFC\uC815\uC740 \uBC84\uADF8\uB97C \uC2DD\uBCC4\uD558\uACE0\
  \ \uC218\uC815\uD558\uC5EC \uCF54\uB4DC\uAC00 \uC608\uC0C1\uB300\uB85C \uB3D9\uC791\
  \uD558\uB3C4\uB85D \uD558\uB294 \uB370 \uD544\uC218\uC801\uC785\uB2C8\uB2E4."
lastmod: 2024-02-18 23:09:06.960276
model: gpt-4-0125-preview
summary: "C\uC5D0\uC11C\uC758 \uB514\uBC84\uAC70\uB294 \uAC1C\uBC1C\uC790\uAC00 \uCF54\
  \uB4DC\uB97C \uB2E8\uACC4\uBCC4\uB85C \uC2E4\uD589\uD558\uACE0, \uBCC0\uC218\uB97C\
  \ \uAC80\uC0AC\uD558\uACE0, \uC2E4\uD589 \uD750\uB984\uC744 \uBAA8\uB2C8\uD130\uB9C1\
  \uD560 \uC218 \uC788\uB3C4\uB85D \uD558\uB294 \uC804\uBB38 \uB3C4\uAD6C\uC785\uB2C8\
  \uB2E4. \uC774 \uACFC\uC815\uC740 \uBC84\uADF8\uB97C \uC2DD\uBCC4\uD558\uACE0 \uC218\
  \uC815\uD558\uC5EC \uCF54\uB4DC\uAC00 \uC608\uC0C1\uB300\uB85C \uB3D9\uC791\uD558\
  \uB3C4\uB85D \uD558\uB294 \uB370 \uD544\uC218\uC801\uC785\uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇과 왜?

C에서의 디버거는 개발자가 코드를 단계별로 실행하고, 변수를 검사하고, 실행 흐름을 모니터링할 수 있도록 하는 전문 도구입니다. 이 과정은 버그를 식별하고 수정하여 코드가 예상대로 동작하도록 하는 데 필수적입니다.

## 사용 방법:

GDB( GNU 디버거)는 C 프로그래밍에서 가장 일반적으로 사용되는 디버거입니다. 아래는 간단한 C 프로그램을 디버깅하기 위해 GDB를 사용하는 간략한 가이드입니다.

먼저 `-g` 플래그를 사용하여 디버깅 정보를 포함시켜 C 프로그램을 컴파일합니다:

```c
gcc -g program.c -o program
```

다음으로, 컴파일된 프로그램과 함께 GDB를 시작합니다:

```bash
gdb ./program
```

이제 GDB 내에서 다양한 명령어를 사용하여 조작할 수 있습니다. 몇 가지 기본 명령어는 다음과 같습니다:

- `break`: 실행을 일시 중지할 지정된 줄이나 함수에 중단점을 설정합니다.
  - 예시: `break 10` 또는 `break main`
- `run`: GDB 내에서 프로그램의 실행을 시작합니다.
- `next`: 함수 내로 들어가지 않고 다음 코드 줄을 실행합니다.
- `step`: 함수 내로 들어가며 다음 코드 줄을 실행합니다.
- `print`: 변수의 값을 표시합니다.
- `continue`: 다음 중단점까지 실행을 재개합니다.
- `quit`: GDB를 종료합니다.

간단한 프로그램을 디버깅하는 예시 세션입니다:

```c
#include <stdio.h>

int main() {
    int i;
    for (i = 0; i < 5; i++) {
        printf("%d\n", i);
    }
    return 0;
}
```

설명한 대로 컴파일하고 GDB를 시작합니다. `break 5`로 `printf` 줄에 중단점을 설정한 다음 `run`을 실행합니다. 루프를 단계별로 진행하기 위해 `next`를 사용하고 루프 변수를 검사하기 위해 `print i`를 사용합니다.

첫 번째 반복 전에 중단점을 설정한 후 샘플 출력:

```
Breakpoint 1, main () at program.c:5
5         printf("%d\n", i);
```

몇 번의 반복 후 `print i`를 사용:

```
$3 = 2
```

이는 간단한 프로그램의 상태와 흐름을 검사하는 방법을 보여줍니다.

## 심층 탐구

디버깅의 개념은 프로그래밍 초기, 물리적 버그(실제 곤충)가 기계식 컴퓨터에서 문제를 일으킬 수 있던 시절부터 크게 진화했습니다. 오늘날, GDB와 같은 디버거는 기본적인 단계 진행 및 변수 검사를 넘어서, 프로그램을 역으로 실행하는 역디버깅, 조건부 중단점, 자동화된 디버깅 작업을 위한 스크립팅과 같은 고급 기능을 제공합니다.

GDB는 강력하고 널리 사용되지만, 초보자에게는 다소 복잡하고 어려울 수 있습니다. Visual Studio Code, CLion, Eclipse와 같은 대체 디버깅 도구 및 통합 개발 환경(IDE)은 보다 사용자 친화적인 인터페이스를 제공하여 C 코드를 디버깅하는 것을 돕습니다. 이들은 종종 시각적 도움말과 더 직관적인 컨트롤을 통합하며, GDB의 모든 기능을 제공하지 않을 수 있지만 C 프로그래밍에 새로운 사용자에게 더 접근하기 쉬울 수 있습니다.

또한, 언어 서버 프로토콜과 디버깅 표준의 등장은 다양한 도구 및 환경에서 보다 일관된 디버깅 경험을 제공하는 크로스 플랫폼 디버깅 솔루션을 가능하게 했습니다. 이러한 발전에도 불구하고 GDB와 같은 전통적인 디버거의 내부를 배우는 것은 C 프로그램의 실행에 대한 귀중한 통찰을 제공하며 개발자의 도구 상자에서 중요한 기술로 남아 있습니다.
