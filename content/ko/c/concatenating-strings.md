---
title:                "문자열 연결하기"
date:                  2024-01-20T17:34:15.380617-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 연결하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜 사용하는가?)
문자열 연결은 여러 문자열을 하나로 합치는 과정입니다. 프로그래머들은 데이터를 형식화하거나 출력 메시지를 생성할 때 이 방법을 사용합니다.

## How to: (방법)
```C
#include <stdio.h>
#include <string.h>

int main() {
    char greeting[20] = "안녕, ";
    char name[10] = "세계!";
    strcat(greeting, name); // greeting에 name을 연결함
    printf("%s\n", greeting); // 연결된 문자열 출력

    return 0;
}
```
출력:
```
안녕, 세계!
```

## Deep Dive (심화 탐구)
문자열 연결은 C 언어의 초기부터 사용되어 온 기법입니다. `strcat` 함수는 문자열을 연결하는 대표적인 방법이지만, 이 함수는 목적지 버퍼가 충분히 크다고 가정합니다. 버퍼 오버플로우를 방지하기 위해 `strncat` 같은 보안 강화 버전을 사용하는 것이 좋습니다. 문자열 연결 성능을 극대화하려면 직접 구현하거나 불변성과 메모리 관리 측면에서 최적화된 대안적 접근 방식을 고려해야 할 수 있습니다.

## See Also (관련 자료)
- C 언어의 `strcat` 함수: https://www.cplusplus.com/reference/cstring/strcat/
- `strncat` 함수에 대한 안전한 사용 방법: https://www.cplusplus.com/reference/cstring/strncat/
- 문자열 핸들링과 관련된 보안 문제들: https://owasp.org/www-community/vulnerabilities/Buffer_Overflow