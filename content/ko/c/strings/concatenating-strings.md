---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:47.727796-07:00
description: "\uBC29\uBC95: C\uC5D0\uC11C, \uBB38\uC790\uC5F4\uC740 \uB110 \uBB38\uC790\
  (`\\0`)\uB85C \uB05D\uB098\uB294 \uBB38\uC790 \uBC30\uC5F4\uC785\uB2C8\uB2E4. \uACE0\
  \uAE09 \uC5B8\uC5B4\uC640 \uB2EC\uB9AC C\uB294 \uB0B4\uC7A5 \uBB38\uC790\uC5F4 \uC5F0\
  \uACB0 \uD568\uC218\uB97C \uC81C\uACF5\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. \uB300\
  \uC2E0, `<string.h>` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC758 `strcat()` \uB610\uB294\
  \ `strncat()` \uD568\uC218\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. `strcat()`\uC744\
  \ \uC0AC\uC6A9\uD55C\u2026"
lastmod: '2024-03-13T22:44:55.909383-06:00'
model: gpt-4-0125-preview
summary: "C\uC5D0\uC11C, \uBB38\uC790\uC5F4\uC740 \uB110 \uBB38\uC790(`\\0`)\uB85C\
  \ \uB05D\uB098\uB294 \uBB38\uC790 \uBC30\uC5F4\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

## 방법:
C에서, 문자열은 널 문자(`\0`)로 끝나는 문자 배열입니다. 고급 언어와 달리 C는 내장 문자열 연결 함수를 제공하지 않습니다. 대신, `<string.h>` 라이브러리의 `strcat()` 또는 `strncat()` 함수를 사용합니다.

`strcat()`을 사용한 간단한 예제입니다:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hello, ";
    char source[] = "World!";

    strcat(destination, source);

    printf("%s\n", destination);  // 출력 결과: Hello, World!
    return 0;
}
```

`strcat()` 함수는 두 개의 인자를 받습니다: 목적지 문자열(연결된 결과를 담을 충분한 공간을 가져야 함)과 원본 문자열입니다. 그러고 나서 원본 문자열을 목적지 문자열에 추가합니다.

연결될 문자의 수를 더 많이 제어하기 위해 `strncat()`을 사용하는 것이 더 안전합니다:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hello, ";
    char source[] = "World!";
    int num = 3; // 추가할 문자의 수

    strncat(destination, source, num);

    printf("%s\n", destination);  // 출력 결과: Hello, Wor
    return 0;
}
```

이 방법은 원본 문자열의 처음 `num` 문자만 연결을 제한함으로써 버퍼 오버플로를 방지하는 데 도움이 됩니다.

## 심화 학습
`strcat()`과 `strncat()` 함수는 C 표준 라이브러리의 초기부터 있었으며, 문자열과 메모리의 수동 관리가 필요한 언어의 저수준 본성을 반영합니다. 많은 현대 프로그래밍 언어들이 문자열을 내장된 연결 연산자(`+` 또는 `.concat()` 등)와 함께 일급 객체로 취급하는 반면, C의 접근 방식은 포인터, 메모리 할당, 버퍼 오버플로와 같은 잠재적인 문제에 대한 깊은 이해를 요구합니다.

`strcat()`과 `strncat()`은 널리 사용되지만, 주의해서 사용하지 않을 경우 보안 취약점을 생성할 수 있다는 점에서 종종 비판을 받습니다. 데이터가 할당된 메모리를 초과하는 버퍼 오버플로는 충돌을 일으키거나 임의 코드 실행을 위해 악용될 수 있습니다. 그 결과 프로그래머들은 목적지 문자열의 크기에 기반하여 문자의 수를 제한함으로써 더 예측 가능한 동작을 제공하는 `snprintf()`와 같은 더 안전한 대안으로 전환하고 있습니다:

```c
char destination[50] = "Hello, ";
char source[] = "World!";
snprintf(destination + strlen(destination), sizeof(destination) - strlen(destination), "%s", source);
```

이 방법은 더 장황하지만 훨씬 더 안전하며, 간결함보다 보안과 견고함을 우선시하는 C 프로그래밍 관행의 변화를 강조합니다.

이러한 도전에도 불구하고 C에서의 문자열 연결은 언어에서 효과적으로 프로그래밍하는 데 필수적인 기술입니다. 그 뉘앙스와 관련된 위험을 이해하는 것은 C 프로그래밍을 마스터하는 데 핵심입니다.
