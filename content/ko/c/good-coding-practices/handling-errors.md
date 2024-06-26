---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:08.100360-07:00
description: "\uBC29\uBC95: C\uB294 \uB2E4\uB978 \uC77C\uBD80 \uC5B8\uC5B4\uC640 \uAC19\
  \uC740 \uC608\uC678\uC5D0 \uB300\uD55C \uB0B4\uC7A5 \uC9C0\uC6D0\uC774 \uC5C6\uC2B5\
  \uB2C8\uB2E4. \uB300\uC2E0, \uD568\uC218\uC5D0\uC11C \uD2B9\uBCC4\uD55C \uAC12\uC744\
  \ \uBC18\uD658\uD558\uAC70\uB098 `errno` \uAC19\uC740 \uC804\uC5ED \uBCC0\uC218\uB97C\
  \ \uC124\uC815\uD558\uB294 \uBA87 \uAC00\uC9C0 \uC804\uD1B5\uC801\uC778 \uC624\uB958\
  \ \uCC98\uB9AC \uC804\uB7B5\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4. **\uD2B9\uBCC4\
  \uD55C \uAC12 \uBC18\uD658\uD558\uAE30** \uD568\uC218\uB294 \uC720\uD6A8\uD55C \uACB0\
  \uACFC\uC77C \uAC00\uB2A5\uC131\uC774 \uB0AE\uC740 \uD2B9\uC815 \uAC12\uC744\u2026"
lastmod: '2024-03-13T22:44:55.936615-06:00'
model: gpt-4-0125-preview
summary: "C\uB294 \uB2E4\uB978 \uC77C\uBD80 \uC5B8\uC5B4\uC640 \uAC19\uC740 \uC608\
  \uC678\uC5D0 \uB300\uD55C \uB0B4\uC7A5 \uC9C0\uC6D0\uC774 \uC5C6\uC2B5\uB2C8\uB2E4\
  ."
title: "\uC624\uB958 \uCC98\uB9AC\uD558\uAE30"
weight: 16
---

## 방법:
C는 다른 일부 언어와 같은 예외에 대한 내장 지원이 없습니다. 대신, 함수에서 특별한 값을 반환하거나 `errno` 같은 전역 변수를 설정하는 몇 가지 전통적인 오류 처리 전략을 사용합니다.

**특별한 값 반환하기**

함수는 유효한 결과일 가능성이 낮은 특정 값을 반환하여 오류를 표시할 수 있습니다. 여기 정수를 사용한 예가 있습니다:

```c
#include <stdio.h>

int inverse(int number, double *result) {
    if (number == 0) {
        return -1; // 오류 상황
    } else {
        *result = 1.0 / number;
        return 0; // 성공
    }
}

int main() {
    double result;
    if (inverse(0, &result) < 0) {
        printf("오류: 0으로 나눔.\n");
    } else {
        printf("역수는: %f\n", result);
    }
    
    return 0;
}
```

**출력:**
```
오류: 0으로 나눔.
```

**`errno` 확인하기**

시스템이나 OS(파일 I/O와 같은)와 상호 작용하는 라이브러리 함수의 경우, `errno`는 오류가 발생하면 설정됩니다. 이를 사용하려면 `errno.h`를 포함하고 의심되는 실패 후 `errno`를 확인하세요:

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
    FILE *file = fopen("nonexistent.txt", "r");
    if (file == NULL) {
        printf("파일을 여는 데 오류: %s\n", strerror(errno));
    }
    
    return 0;
}
```

**출력:**
```
파일을 여는 데 오류: 그런 파일이나 디렉터리가 없습니다
```

## 심층 탐구
역사적으로, C 프로그래밍 언어의 최소주의적 디자인은 내장된 예외 처리 메커니즘을 배제하였는데, 이는 최대 성능과 금속에 가까운 제어가 중요한 저수준 시스템 프로그래밍의 기원을 반영합니다. 대신, C는 프로그래머에게 가능한 많은 제어를 제공하는 철학에 맞는 수동 오류 처리 접근 방식을 채택합니다. 이는 편의성의 비용이 들더라도 말입니다.

이 접근 방식은 C의 설계 목표와 잘 어울리지만, 동시에 장황한 오류 검사 코드를 초래하고 오류 검사를 놓칠 가능성이 있으며, 현대 언어는 구조화된 예외 처리 메커니즘으로 이를 해결합니다. 예를 들어, Java 또는 C#과 같은 언어에서의 예외는 중앙 집중식 오류 처리를 허용하여 코드를 더 깔끔하게 만들고 오류 관리를 더 단순하게 만듭니다. 하지만, 예외는 오버헤드와 복잡성을 도입하는데, 이는 C가 빛나는 시스템 수준 프로그래밍에는 이상적이지 않을 수 있습니다.

그럼에도 불구하고, C에서의 이 수동 오류 처리는 많은 다른 언어들의 오류 관리 설계에 영향을 미쳐, 오류 조건의 명시성이 더 예측 가능하고 디버깅하기 쉬운 코드로 이어질 수 있는 모델을 제공합니다. 실패를 우아하게 관리해야 하는 중요 시스템의 경우, C의 오류 처리 패러다임은 최신 모범 사례(오류 처리 라이브러리 및 규칙과 같은)와 결합할 때, 견고성과 신뢰성을 보장합니다.
