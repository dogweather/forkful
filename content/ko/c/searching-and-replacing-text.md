---
title:                "텍스트 검색 및 교체"
aliases:
- ko/c/searching-and-replacing-text.md
date:                  2024-02-03T18:08:39.526770-07:00
model:                 gpt-4-0125-preview
simple_title:         "텍스트 검색 및 교체"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/searching-and-replacing-text.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

C에서 텍스트 검색 및 교체는 더 큰 문자열 내에서 특정 부분 문자열을 식별하고 이를 다른 부분 문자열로 대체하는 것을 포함합니다. 프로그래머들은 데이터 정제 및 형식 지정에서 동적으로 콘텐츠를 생성하는 작업에 이르기까지 텍스트 데이터를 조작하기 위해 이러한 작업을 수행합니다.

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
