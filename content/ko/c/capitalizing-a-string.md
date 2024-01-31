---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
simple_title:         "문자열 대문자로 변환하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
문자열을 소문자에서 대문자로 바꾸는 것입니다. 데이터의 일관성을 위해 또는 사용자 인터페이스의 가독성을 높이기 위해 프로그래머는 이를 수행합니다.

## How to: (어떻게 하나요?)
```C
#include <stdio.h>
#include <ctype.h>

void capitalize(char *str) {
    while(*str) {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char text[] = "hello, world!";
    capitalize(text);
    printf("Capitalized String: %s\n", text);
    return 0;
}
```
Output:
```
Capitalized String: HELLO, WORLD!
```

## Deep Dive (심화 탐구)
대소문자 변환은 고전 C 시대부터 있었습니다. 'ctype.h'는 문자 변환 함수를 제공하며, 이는 오래전부터 표준 라이브러리의 일부입니다. 대안으로는 ASCII 코드를 이용한 변환이 있으나, 'toupper' 함수를 사용하는 것이 더 간편하고 에러를 줄일 수 있습니다. 구현 상세로는, 문자열을 배열 혹은 포인터로 관리할 수 있고, ASCII 값 조작을 통해 직접 변환할 수도 있지만, 국제화 지원 면에서 'ctype.h' 라이브러리의 함수를 사용하는 것이 더 나을 수 있습니다.

## See Also (관련 자료)
- C Standard Library documentation on `toupper`: https://en.cppreference.com/w/c/string/byte/toupper
- ASCII table and description: https://www.asciitable.com/
- C string handling: https://en.cppreference.com/w/c/string/byte
