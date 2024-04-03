---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:35.320801-07:00
description: "\uC5B4\uB5BB\uAC8C: \uC77C\uBD80 \uACE0\uAE09 \uC5B8\uC5B4\uAC00 \uBD80\
  \uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C\uC744 \uC704\uD55C \uB0B4\uC7A5 \uBA54\uC18C\
  \uB4DC\uB97C \uC81C\uACF5\uD558\uB294 \uAC83\uACFC \uB2EC\uB9AC, C\uB294 \uBB38\uC790\
  \uC5F4 \uC870\uC791 \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uB294 \uB354 \uC218\uB3D9\
  \uC801\uC778 \uC811\uADFC \uBC29\uC2DD\uC744 \uC694\uAD6C\uD569\uB2C8\uB2E4. \uB2E4\
  \uC74C\uC740 C\uC5D0\uC11C \uD6A8\uACFC\uC801\uC73C\uB85C \uBD80\uBD84 \uBB38\uC790\
  \uC5F4\uC744 \uCD94\uCD9C\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4: #."
lastmod: '2024-03-13T22:44:55.904095-06:00'
model: gpt-4-0125-preview
summary: "\uC77C\uBD80 \uACE0\uAE09 \uC5B8\uC5B4\uAC00 \uBD80\uBD84 \uBB38\uC790\uC5F4\
  \ \uCD94\uCD9C\uC744 \uC704\uD55C \uB0B4\uC7A5 \uBA54\uC18C\uB4DC\uB97C \uC81C\uACF5\
  \uD558\uB294 \uAC83\uACFC \uB2EC\uB9AC, C\uB294 \uBB38\uC790\uC5F4 \uC870\uC791\
  \ \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uB294 \uB354 \uC218\uB3D9\uC801\uC778 \uC811\
  \uADFC \uBC29\uC2DD\uC744 \uC694\uAD6C\uD569\uB2C8\uB2E4."
title: "\uBD80\uBD84 \uBB38\uC790\uC5F4 \uCD94\uCD9C\uD558\uAE30"
weight: 6
---

## 어떻게:
일부 고급 언어가 부분 문자열 추출을 위한 내장 메소드를 제공하는 것과 달리, C는 문자열 조작 함수를 사용하는 더 수동적인 접근 방식을 요구합니다. 다음은 C에서 효과적으로 부분 문자열을 추출하는 방법입니다:

### 예제 1: `strncpy` 사용하기
```c
#include <stdio.h>
#include <string.h>

int main() {
    char text[] = "Hello, World!";
    char buffer[20];

    // "Hello, World!"에서 "World" 추출
    strncpy(buffer, text + 7, 5);
    buffer[5] = '\0'; // 널 종결 보장

    printf("추출된 부분 문자열: %s\n", buffer);
    // 출력: 추출된 부분 문자열: World
    return 0;
}
```

### 예제 2: 함수 생성하기
반복적인 사용을 위해, 부분 문자열을 추출하기 위한 전용 함수를 만드는 것이 더 효율적일 수 있습니다:

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void extractSubstring(char *source, int from, int n, char *target) {
    strncpy(target, source + from, n);
    target[n] = '\0'; // 널 종결 보장
}

int main() {
    char text[] = "Programming in C";
    char buffer[50];

    extractSubstring(text, 0, 11, buffer);
    printf("추출된 부분 문자열: %s\n", buffer);
    // 출력: 추출된 부분 문자열: Programming
    return 0;
}
```

## 심층 분석
C에서 부분 문자열을 추출하는 것은 주로 포인터 조작과 신중한 메모리 관리를 통해 처리되며, 이는 데이터를 처리하는 데 있어 언어의 저수준 접근 방식을 반영합니다. 이 방법은 C 프로그래밍의 초기 시절로 거슬러 올라가며, 제한된 컴퓨팅 파워로 인해 자원을 효율적으로 관리하는 것이 매우 중요했습니다. 내장된 부분 문자열 함수가 없는 것이 누락처럼 보일 수 있지만, 프로그래머에게 메모리 관리에 대한 완전한 제어권을 부여하는 C의 철학을 예시합니다. 이는 종종 최적화되었지만 더 복잡한 코드로 이어집니다.

현대 프로그래밍의 영역에서는 파이썬과 자바스크립트와 같은 언어가 `slice()` 또는 인덱스를 사용한 문자열 슬라이싱과 같은 부분 문자열 추출을 위한 내장 메소드를 제공합니다. 이러한 고급 언어는 메모리 관리를 백그라운드에서 처리하며, 사용 편의성과 가독성을 위해 일부 제어 정도를 희생합니다.

C 프로그래머들에게, 포인터 산술 및 메모리 할당에 대한 이해는 부분 문자열 추출과 같은 작업을 위해 필수적입니다. 이 접근법은 문자열이 메모리에서 어떻게 표현되고 조작되는지에 대한 더 깊은 이해를 요구하지만, 비교할 수 없는 제어와 효율성을 제공합니다. 이는 수십 년 동안 성능이 중요한 응용 프로그램에서 C 프로그래밍을 관련성 있게 유지한 특징적인 특성입니다. 그러나 직접적인 메모리 관리가 덜 중요한 고급 응용 프로그램을 작업하는 사람들에게는 내장된 부분 문자열 기능을 갖춘 언어가 더 간단하고 오류가 덜 발생하는 접근 방식을 제공할 수 있습니다.
