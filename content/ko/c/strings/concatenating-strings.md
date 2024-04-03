---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:47.727796-07:00
description: "C\uC5D0\uC11C\uC758 \uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uB450 \uAC1C\
  \ \uC774\uC0C1\uC758 \uBB38\uC790\uC5F4\uC744 \uB05D\uACFC \uB05D\uC73C\uB85C \uC5F0\
  \uACB0\uD558\uC5EC \uC0C8 \uBB38\uC790\uC5F4\uC744 \uD615\uC131\uD558\uB294 \uC791\
  \uC5C5\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB7F0\uD0C0\
  \uC784\uC5D0 \uB3D9\uC801\uC73C\uB85C \uBB38\uC790\uC5F4\uC744 \uAD6C\uC131\uD558\
  \uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uB294\uB370, \uC774\
  \uB294 \uC758\uBBF8 \uC788\uB294 \uBA54\uC2DC\uC9C0, \uD30C\uC77C \uACBD\uB85C \uB610\
  \uB294 \uB2E4\uC591\uD55C \uBB38\uC790\uC5F4 \uC18C\uC2A4\uC5D0\uC11C \uC870\uB9BD\
  \uB41C \uB370\uC774\uD130\uB97C \uC0DD\uC131\uD558\uB294 \uB370\u2026"
lastmod: '2024-03-13T22:44:55.909383-06:00'
model: gpt-4-0125-preview
summary: "C\uC5D0\uC11C\uC758 \uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uB450 \uAC1C\
  \ \uC774\uC0C1\uC758 \uBB38\uC790\uC5F4\uC744 \uB05D\uACFC \uB05D\uC73C\uB85C \uC5F0\
  \uACB0\uD558\uC5EC \uC0C8 \uBB38\uC790\uC5F4\uC744 \uD615\uC131\uD558\uB294 \uC791\
  \uC5C5\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

## 무엇 & 왜?

C에서의 문자열 연결은 두 개 이상의 문자열을 끝과 끝으로 연결하여 새 문자열을 형성하는 작업입니다. 프로그래머들은 런타임에 동적으로 문자열을 구성하기 위해 이 작업을 수행하는데, 이는 의미 있는 메시지, 파일 경로 또는 다양한 문자열 소스에서 조립된 데이터를 생성하는 데 필수적입니다.

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
