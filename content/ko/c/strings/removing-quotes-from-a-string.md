---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:18.917753-07:00
description: "\uBC29\uBC95: C\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\
  \uD45C\uB97C \uC81C\uAC70\uD558\uB824\uBA74 \uBB38\uC790\uC5F4\uC744 \uC21C\uD68C\
  \uD558\uBA74\uC11C \uB530\uC634\uD45C\uAC00 \uC544\uB2CC \uBB38\uC790\uB97C \uC0C8\
  \ \uBB38\uC790\uC5F4\uC5D0 \uBCF5\uC0AC\uD569\uB2C8\uB2E4. \uC774 \uACFC\uC815\uC740\
  \ \uBB38\uC790\uC5F4\uC5D0\uC11C \uC120\uD589 \uBC0F \uD6C4\uD589 \uB530\uC634\uD45C\
  \uB9CC\uC744 \uC81C\uAC70\uD558\uAC70\uB098 \uBB38\uC790\uC5F4\uC5D0 \uC874\uC7AC\
  \uD558\uB294 \uBAA8\uB4E0 \uB530\uC634\uD45C\uB97C \uC81C\uAC70\uD558\uB294 \uB370\
  \ \uC870\uC815\uB420 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC544\uB798\uB294 \uB450\
  \ \uC811\uADFC \uBC29\uC2DD\uC744 \uBAA8\uB450 \uBCF4\uC5EC\uC8FC\uB294\u2026"
lastmod: '2024-03-13T22:44:55.902205-06:00'
model: gpt-4-0125-preview
summary: "C\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\
  \uAC70\uD558\uB824\uBA74 \uBB38\uC790\uC5F4\uC744 \uC21C\uD68C\uD558\uBA74\uC11C\
  \ \uB530\uC634\uD45C\uAC00 \uC544\uB2CC \uBB38\uC790\uB97C \uC0C8 \uBB38\uC790\uC5F4\
  \uC5D0 \uBCF5\uC0AC\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
weight: 9
---

## 방법:
C에서 문자열에서 따옴표를 제거하려면 문자열을 순회하면서 따옴표가 아닌 문자를 새 문자열에 복사합니다. 이 과정은 문자열에서 선행 및 후행 따옴표만을 제거하거나 문자열에 존재하는 모든 따옴표를 제거하는 데 조정될 수 있습니다. 아래는 두 접근 방식을 모두 보여주는 예시입니다:

```c
#include <stdio.h>
#include <string.h>

// 문자열에서 모든 따옴표를 제거하는 함수
void removeAllQuotes(char *source, char *dest) {
    while (*source) {
        if (*source != '"' && *source != '\'') {
            *dest++ = *source;
        }
        source++;
    }
    *dest = '\0'; // 목적지 문자열을 Null로 종료
}

// 문자열에서 선행 및 후행 따옴표만 제거하는 함수
void removeEdgeQuotes(char *source, char *dest) {
    size_t len = strlen(source);
    if (source[0] == '"' || source[0] == '\'') source++, len--;
    if (source[len-1] == '"' || source[len-1] == '\'') len--;
    strncpy(dest, source, len);
    dest[len] = '\0'; // 목적지 문자열을 Null로 종료
}

int main() {
    char str1[] = "'Hello, World!'";
    char str2[] = "\"Programming in C\"";
    char noQuotes1[50];
    char noQuotes2[50];
    
    removeAllQuotes(str1, noQuotes1);
    printf("All Quotes Removed: %s\n", noQuotes1);
    
    removeEdgeQuotes(str2, noQuotes2);
    printf("Edge Quotes Removed: %s\n", noQuotes2);
    
    return 0;
}
```
샘플 출력:
```
All Quotes Removed: Hello, World!
Edge Quotes Removed: Programming in C
```

이 예시들은 문자열에 존재하는 모든 따옴표를 제거하는 처리와 선행 및 후행 따옴표만을 대상으로 하는 제거 방법을 보여줍니다.

## 심층 탐구
문자열에서 따옴표를 제거하는 개념은 초기 텍스트 처리 요구와 연관된 것을 제외하고 C에서 상당한 역사적 깊이가 있는 것은 아닙니다. 여기서 보여진 간단한 접근 방식은 다재다능하지만 매우 큰 문자열이나 고성능 요구 사항에 대해 효율적이지 않으며, 대신 제자리 수정 또는 보다 고급 알고리즘이 선호될 수 있습니다.

따옴표를 찾아서 문자열의 따옴표가 아닌 부분을 이동하는 `strpbrk`와 같은 대안은 더 효율적일 수 있지만 C에서 포인터와 메모리 관리에 대한 더 깊은 이해를 요구합니다. 게다가, 정규 표현식 라이브러리의 출현은 따옴표 제거를 포함한 문자열 조작을 위한 강력한 도구 세트를 제공했습니다. 그러나, 이러한 라이브러리는 강력하지만, 간단한 작업에 필요하지 않은 복잡성과 오버헤드를 추가할 수 있습니다. 결과적으로, 여기서 보여진 직접 접근 방식은 C 프로그래머에게 많은 일반적인 사용 사례를 위한 단순함과 효율성을 결합하는 귀중한 기술로 남아 있습니다.
