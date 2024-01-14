---
title:                "C: 패턴과 일치하는 문자 삭제하기"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

일부 사용자는 특정 패턴과 일치하는 문자를 삭제하는 것이 필요할 수 있습니다.

## 사용 방법

아래 코드 블록에서는 문자열에서 특정 패턴과 일치하는 모든 문자를 삭제하는 예제를 보여줍니다.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[50] = "Hello World! This is a sample string.";
    char pattern[10] = "is";

    // pattern과 일치하는 문자를 삭제
    char *ptr = strstr(str, pattern);
    while(ptr) {
        memmove(ptr, ptr + strlen(pattern), strlen(ptr + strlen(pattern)) + 1);
        ptr = strstr(str, pattern);
    }

    printf("%s", str); // "Hello World! Th  a sample string."

    return 0;
}
```

위 코드에서는 `strstr()` 함수를 사용하여 해당 패턴을 찾고, `memmove()` 함수를 사용하여 해당 패턴과 일치하는 문자를 삭제했습니다.

## 심층 탐구

문자를 삭제하는 작업은 문자열 처리에서 일반적으로 수행되는 작업 중 하나입니다. 여러분은 C 언어에서 다양한 방법을 사용하여 문자를 삭제할 수 있습니다. 위 예제에서는 `memmove()` 함수를 사용했지만, `strncpy()` 함수를 사용하는 방법이나 포인터를 이용하여 문자를 삭제하는 방법도 있습니다.

## 더 알아보기

- [strncpy() 함수에 대한 자세한 설명](https://www.geeksforgeeks.org/strncpy-c-function/)
- [포인터를 이용한 문자 삭제 방법에 대한 설명](https://www.educative.io/edpresso/how-to-delete-a-specific-character-from-a-string-in-c)
- [C 언어의 문자열 함수들에 대한 전체 목록](https://www.tutorialspoint.com/c_standard_library/string_h.htm)