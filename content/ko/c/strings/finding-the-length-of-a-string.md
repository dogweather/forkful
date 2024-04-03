---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:42.641716-07:00
description: "\uBC29\uBC95: C\uC5D0\uC11C\uB294 `strlen()` \uD45C\uC900 \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC \uD568\uC218\uB97C \uD754\uD788 \uC0AC\uC6A9\uD558\uC5EC \uBB38\
  \uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uC2B5\uB2C8\uB2E4. \uB2E4\uC74C\uC740\
  \ \uAC04\uB2E8\uD55C \uC608\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.907651-06:00'
model: gpt-4-0125-preview
summary: "C\uC5D0\uC11C\uB294 `strlen()` \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \ \uD568\uC218\uB97C \uD754\uD788 \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\uC758\
  \ \uAE38\uC774\uB97C \uCC3E\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

## 방법:
C에서는 `strlen()` 표준 라이브러리 함수를 흔히 사용하여 문자열의 길이를 찾습니다. 다음은 간단한 예입니다:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "Hello, World!";
    size_t length = strlen(myString);
    
    printf("Length of '%s' is %zu.\n", myString, length);
    
    return 0;
}
```

**샘플 출력:**
```
'Hello, World!'의 길이는 13입니다.
```

이 예제에서 `strlen()`은 문자열(`myString`)을 입력으로 받아 널 종료자를 제외한 길이를 반환합니다. 길이 변수에 `size_t`를 사용하는 것이 권장됩니다. 왜냐하면 이는 부호 없는 정수 타입이기 때문에 시스템에서 가능한 가장 큰 객체의 크기를 나타낼 수 있기 때문입니다.

## 심화 탐구:
`strlen()` 함수는 C 언어가 처음 생겼을 때부터 C 표준 라이브러리의 일부였습니다. 내부적으로는 널 종료자를 만날 때까지 문자열을 따라가며 카운터를 증가시킴으로써 작동합니다. 하지만, 이러한 단순성에는 성능 고려 사항이 따릅니다: `strlen()`은 런타임에 문자 수를 계산하기 때문에, 예를 들어 반복문에서 같은 문자열에 대해 반복해서 호출하는 것은 비효율적입니다.

보안 측면에서 `strlen()`과 다른 C 문자열 처리 함수들은 기본적으로 버퍼 오버런을 확인하지 않습니다. 따라서 취약점을 피하려면 주의 깊은 프로그래밍이 필수적입니다. 다른 언어의 현대적인 대안, 예를 들어 길이를 포함하는 문자열 타입이나 기본적으로 안전한 버퍼 처리를 사용하는 것은 이러한 위험과 비효율성을 일부 제거합니다.

한계에도 불구하고, C에서의 `strlen()` 및 수동 문자열 처리에 대한 이해는 특히 저수준 코드를 작업할 때나 성능 및 메모리 제어가 매우 중요할 때 프로그래머에게 중요합니다. 또한 다른 언어에서의 상위 수준 문자열 추상화의 작동 방식에 대한 귀중한 통찰을 제공합니다.
