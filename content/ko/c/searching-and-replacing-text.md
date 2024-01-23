---
title:                "텍스트 검색 및 교체"
date:                  2024-01-20T17:57:15.899943-07:00
model:                 gpt-4-1106-preview
simple_title:         "텍스트 검색 및 교체"
programming_language: "C"
category:             "C"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 그리고 왜?)
텍스트 검색 및 교체는 주어진 문자열 속에서 특정 문자열을 찾아 다른 문자열로 바꾸는 것입니다. 프로그래머들은 데이터 정리, 소스 코드 수정, 자동화된 대량 편집 등을 위해 이를 사용합니다.

## How to: (어떻게 하나요?)
``` C
#include <stdio.h>
#include <string.h>

void searchAndReplace(char *text, const char *search, const char *replace) {
    char buffer[1024];
    char *pos;
    int index = 0;
    int searchLen = strlen(search);
    int replaceLen = strlen(replace);

    while ((pos = strstr(text, search)) != NULL) {
        strncpy(buffer + index, text, pos - text);
        index += pos - text;
        strcpy(buffer + index, replace);
        index += replaceLen;
        text = pos + searchLen;
    }
    strcpy(buffer + index, text);
    strcpy(text, buffer);
}

int main() {
    char text[] = "Hello, World! The World is big.";
    const char search[] = "World";
    const char replace[] = "Earth";

    printf("Before: %s\n", text);
    searchAndReplace(text, search, replace);
    printf("After: %s\n", text);
    return 0;
}
```
Sample Output:
```
Before: Hello, World! The World is big.
After: Hello, Earth! The Earth is big.
```

## Deep Dive (심층 분석)
텍스트 검색 및 교체는 컴퓨팅의 초창기부터 사용되어 왔습니다. 초창기에는 플랫 파일 데이터와 코드에서 사용되는 정규 표현식을 통한 검색 및 교체 기능이 주를 이뤘죠. `strstr`, `strncpy` 같은 C 표준 라이브러리 함수들을 사용함으로써, 간단한 검색 및 교체 기능을 구현할 수 있습니다만, 이는 정규 표현식같이 복잡한 패턴을 매칭할 수는 없습니다. 복잡한 패턴이 필요할 때는 PCRE (Perl Compatible Regular Expressions) 라이브러리나 다른 정규 표현식 라이브러리를 사용하세요. 또한, 대용량 텍스트 처리를 위해서는 버퍼링, 메모리 관리, 문자 인코딩 등 추가적인 고려가 필요합니다.

## See Also (참고 자료)
- C Standard Library Functions: https://en.cppreference.com/w/c/string/byte
- PCRE Library: https://www.pcre.org/
- Regular Expressions in GNU C Library: https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html
- Character Encoding Standards: https://unicode.org/standard/standard.html
