---
title:                "문자열을 소문자로 변환하기"
date:                  2024-01-20T17:38:12.290249-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열을 소문자로 변환한다는 것은, 프로그램 내의 모든 대문자를 소문자로 바꾸는 것을 말합니다. 이것은 대소문자 구분 없이 데이터를 비교하거나 정렬할 때 필요합니다.

## How to: (방법)
C언어에서는 `tolower` 함수를 사용하여 간단하게 문자열을 소문자로 변환할 수 있습니다. 여기서는 `string.h` 헤더 파일의 `strlen` 함수와 함께 사용하는 예제를 보여드릴게요:

```c
#include <stdio.h>
#include <ctype.h>
#include <string.h>

void convertToLowercase(char *s) {
    for(int i = 0; i < strlen(s); i++) {
        s[i] = tolower((unsigned char)s[i]);
    }
}

int main() {
    char myString[] = "Hello, World!";
    convertToLowercase(myString);
    printf("%s\n", myString);
    return 0;
}
```

예상 출력:
```
hello, world!
```

## Deep Dive (심층 분석)
C언어의 `tolower` 함수는 C 표준 라이브러리에 포함된 함수 중 하나입니다. 이 함수의 입력으로 주어진 대문자 하나를 소문자로 변환합니다. 원래 ASCII 코드를 기반으로 만들어진 C언어에서는 대소문자의 변환을 아스키 값의 변화를 통하여 수행했습니다.

대안으로는 ASCII값을 직접 조작하여 변환하는 방법도 있지만, 이는 로케일에 따라 달라질 수 있는 문자 인코딩을 고려하지 않으므로 권장하지 않습니다. 또한, C99 표준부터는 `toupper`와 `tolower` 함수가 멀티바이트 문자에 대해서도 올바르게 동작하도록 개선되었습니다.

다만, 주의할 점으로는 `tolower` 함수는 int 타입을 인자로 받아들이며, EOF를 제대로 처리하기 위해 unsigned char로 형 변환하는 것이 좋습니다. 이렇게 형 변환을 거치지 않으면 음의 값이 들어왔을 때, 함수가 정의되지 않은 동작을 할 수 있습니다.

## See Also (참조)
- C 표준 라이브러리 매뉴얼: https://en.cppreference.com/w/c/string/byte/tolower
- 문자열 처리 함수에 대한 더 깊은 이해를 위한: https://en.cppreference.com/w/c/string/byte
- 로케일과 문자 인코딩에 대한 추가 정보: https://en.cppreference.com/w/c/locale/locale