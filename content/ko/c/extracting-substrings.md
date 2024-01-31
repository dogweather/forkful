---
title:                "부분 문자열 추출"
date:                  2024-01-20T17:45:18.340220-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"

category:             "C"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜 필요한가요?)
문자열의 일부를 추출하는 것입니다. 데이터 처리, 검색 및 사용자 입력 처리에 요긴합니다.

## How to (방법):
```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hello, world!";
    char substr[6]; // 잠깐, 'Hello'라는 부분 문자열을 위한 공간
    
    strncpy(substr, str, 5); // 'Hello'라는 5개 문자 복사
    substr[5] = '\0'; // null 문자로 종료
    
    printf("Original String: '%s'\n", str);
    printf("Substring: '%s'\n", substr);
    
    return 0;
}
```
출력:
```
Original String: 'Hello, world!'
Substring: 'Hello'
```

## Deep Dive (심층 분석):
부분 문자열을 얻기 위해 C 표준 라이브러리는 지정된 길이만큼 문자를 복사하는 `strncpy` 함수를 제공합니다. 이런 방식은 역사적으로 낮은 레벨의 문자열 조작을 반영합니다. 대안으로 `strndup` (POSIX)이 있지만, 모든 환경에서 사용 가능하지는 않습니다. 구현 시 주의할 점은 경계 체크입니다. 문자열의 끝을 넘어서는 경우, 버퍼 오버플로우가 발생할 수 있습니다. 또한, 적절한 null 종료를 보장해야 합니다.

## See Also (참조):
- C 표준 라이브러리 문서: https://en.cppreference.com/w/c/string/byte/strncpy
- 문자열 조작에 관한 교육적 자료: https://www.learn-c.org/en/Strings
- POSIX 함수 strndup에 대한 정보: http://man7.org/linux/man-pages/man3/strndup.3.html
