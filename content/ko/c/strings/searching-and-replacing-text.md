---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:39.526770-07:00
description: "\uBC29\uBC95: C\uB294 \uBB38\uC790\uC5F4\uC5D0 \uB300\uD574 \uAC80\uC0C9\
  \ \uBC0F \uAD50\uCCB4\uB97C \uC9C1\uC811 \uC218\uD589\uD558\uB294 \uB0B4\uC7A5 \uD568\
  \uC218\uB97C \uC81C\uACF5\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. \uADF8\uB7EC\uB098\
  \ `<string.h>` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC5D0 \uC788\uB294 \uB2E4\uC591\uD55C\
  \ \uBB38\uC790\uC5F4 \uCC98\uB9AC \uD568\uC218\uC640 \uC77C\uBD80 \uC0AC\uC6A9\uC790\
  \ \uC9C0\uC815 \uB85C\uC9C1\uC744 \uACB0\uD569\uD558\uC5EC \uC774\uB97C \uB2EC\uC131\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC544\uB798\uB294 \uBB38\uC790\uC5F4 \uB0B4\
  \uC5D0\uC11C \uBD80\uBD84 \uBB38\uC790\uC5F4\uC744 \uAC80\uC0C9\uD558\uACE0\u2026"
lastmod: '2024-03-13T22:44:55.896806-06:00'
model: gpt-4-0125-preview
summary: "C\uB294 \uBB38\uC790\uC5F4\uC5D0 \uB300\uD574 \uAC80\uC0C9 \uBC0F \uAD50\
  \uCCB4\uB97C \uC9C1\uC811 \uC218\uD589\uD558\uB294 \uB0B4\uC7A5 \uD568\uC218\uB97C\
  \ \uC81C\uACF5\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

## 방법:
C는 문자열에 대해 검색 및 교체를 직접 수행하는 내장 함수를 제공하지 않습니다. 그러나 `<string.h>` 라이브러리에 있는 다양한 문자열 처리 함수와 일부 사용자 지정 로직을 결합하여 이를 달성할 수 있습니다. 아래는 문자열 내에서 부분 문자열을 검색하고 교체하는 기본 예시입니다. 간략히 설명하기 위해 이 예시는 충분한 버퍼 크기를 가정하며 프로덕션 코드에서 고려해야 할 메모리 할당 문제는 처리하지 않습니다.

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void replaceSubstring(char *source, char *sub, char *new_sub) {
    char buffer[1024];
    char *insert_point = &buffer[0];
    const char *tmp = source;
    size_t len_sub = strlen(sub), len_new_sub = strlen(new_sub);
    size_t len_up_to_match;

    while ((tmp = strstr(tmp, sub))) {
        // 매치까지의 길이 계산
        len_up_to_match = tmp - source;
        
        // 매치 전 부분 복사
        memcpy(insert_point, source, len_up_to_match);
        insert_point += len_up_to_match;
        
        // 새로운 부분 문자열 복사
        memcpy(insert_point, new_sub, len_new_sub);
        insert_point += len_new_sub;
        
        // 소스 문자열에서 매치를 지나 다음으로 이동
        tmp += len_sub;
        source = tmp;
    }
    
    // 소스 문자열의 남은 부분 복사
    strcpy(insert_point, source);
    
    // 수정된 문자열 출력
    printf("Modified string: %s\n", buffer);
}

int main() {
    char sourceStr[] = "Hello, this is a test. This test is simple.";
    char sub[] = "test";
    char newSub[] = "sample";
    
    replaceSubstring(sourceStr, sub, newSub);
    
    return 0;
}
```

샘플 출력:
```
Modified string: Hello, this is a sample. This sample is simple.
```

이 코드는 `strstr` 함수를 사용하여 소스 문자열의 모든 인스턴스(`sub`)를 찾아 다른 부분 문자열(`newSub`)로 교체하는 간단한 접근 방식을 보여줍니다. 복잡한 시나리오, 예를 들어 겹치는 부분 문자열과 같은 것을 처리하지 않는 매우 기본적인 예시입니다.

## 심층 탐구
"How to" 섹션에서 사용된 접근 방식은 기본적이며, C에서 제3자 라이브러리 없이 텍스트 검색 및 교체를 달성하는 방법을 보여줍니다. 역사적으로, C가 낮은 수준의 메모리 관리와 성능에 중점을 두기 때문에, 그것의 표준 라이브러리는 Python이나 JavaScript와 같은 언어에서 찾을 수 있는 고급 문자열 조작 기능을 캡슐화하지 않습니다. 프로그래머들은 메모리를 수동으로 관리하고 다양한 문자열 작업을 결합하여 원하는 결과를 달성해야 하며, 이는 복잡성을 증가시키지만 더 많은 제어와 효율성을 제공합니다.

이 수동적 접근 방식은 특히 메모리 할당 및 버퍼 크기를 관리할 때 오류가 발생하기 쉽다는 점을 주목하는 것이 중요합니다. 잘못된 처리는 버퍼 오버플로우와 메모리 손상으로 이어질 수 있으며, 코드를 보안 위험에 노출시킬 수 있습니다.

특히 복잡한 텍스트 처리를 요구하는 많은 실제 시나리오에서는 PCRE(Perl 호환 정규 표현식)과 같은 정규 표현식 기반 검색 및 교체를 위한 제3자 라이브러리를 통합하는 것이 오류 가능성을 줄이고 코드를 단순화하는 데 종종 가치가 있습니다. 또한, 현대 C 표준과 컴파일러는 문자열 조작을 위한 내장 함수와 더 안전한 대안을 점차 제공하여, 이전 C 코드베이스에서 관찰된 공통적인 문제점을 완화하는 것을 목표로 합니다. 그러나 특히 성능 중요 애플리케이션을 최적화할 때 프로그래머의 도구 상자에서 수동 텍스트 처리의 기본 이해는 여전히 가치 있는 기술입니다.
