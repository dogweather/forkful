---
title:                "문자열의 길이 찾기"
date:                  2024-01-20T17:46:55.293462-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열의 길이 찾기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 길이 찾기란, 문자열에 포함된 문자의 수를 찾는 것입니다. 프로그래머는 데이터 처리나 검증 시 이 정보를 활용합니다.

## How to: (어떻게 하나요?)
```c
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "안녕하세요";
    int length = strlen(myString);
    
    printf("The length of the string is: %d\n", length);
    return 0;
}
```
출력은 바이트 단위의 길이를 보여줍니다. "안녕하세요"는 10바이트입니다 (ASCII 기준).
```c
The length of the string is: 10
```

## Deep Dive (심층 분석)
문자열 길이를 찾는 방법은 C 언어에서 오래전부터 사용되어 왔습니다. `strlen` 함수는 `<string.h>` 라이브러리에 정의되어 있으며, 널 종료 문자 ('\0')를 만날 때까지 문자를 세어 길이를 찾습니다. 널 종료 문자는 문자열의 끝을 나타내는 표시입니다.

대안으로, 문자열을 순회하며 직접 길이를 세는 방법도 있으나, 성능이 `strlen`보다 떨어질 수 있습니다. C++이나 다른 언어들은 문자열에 길이를 저장하는 메커니즘을 갖고 있지만, C 언어는 그렇지 않습니다.

`strlen`은 바이트 단위로 길이를 측정합니다. 이는 멀티바이트 문자(예: 한글)를 처리할 때 길이가 의도하지 않게 길게 측정될 수 있음을 의미합니다. C11 표준에서는 멀티바이트 문자열을 처리할 수 있는 함수들을 추가하였지만, 여전히 많은 프로그램들이 기본적인 `strlen`을 사용합니다.

## See Also (더 보기)
- C 표준 라이브러리 설명서: [https://en.cppreference.com/w/c/string/byte/strlen](https://en.cppreference.com/w/c/string/byte/strlen)
- 문자열 처리에 대한 더 깊은 이해를 위한 페이지: [https://www.tutorialspoint.com/cprogramming/c_strings.htm](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- 멀티바이트 문자열 처리에 대한 정보: [https://en.cppreference.com/w/c/string/multibyte](https://en.cppreference.com/w/c/string/multibyte)
