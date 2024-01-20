---
title:                "문자열 대문자화"
html_title:           "C: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜? (What & Why?)
문자열 대문자 변환은 모든 알파벳 문자를 대문자로 바꾸는 것입니다. 이는 사용자 입력의 일관성을 보장하거나 대소문자를 구분하지 않는 검색을 위해 프로그래머들이 주로 사용합니다.

## 어떻게 하나요? (How to)
```C
#include <ctype.h>
#include <stdio.h>

void string_uppercase(char s[]) {
    int i = 0;
    while (s[i]) {
        s[i] = toupper((unsigned char) s[i]);
        i++;
    }
}

int main() {
    char string[] = "hello, world!";
    string_uppercase(string);
    printf("%s\n", string);  // Outputs: HELLO, WORLD!
    return 0;
}
```
위의 코드는 간단히 c 문자열을 대문자로 변환하는 방법을 보여줍니다.

## 깊게 알아보기 (Deep Dive)
문자열 대문자 변환은 ANSI C 표준 이후부터 사용할 수 있었고, 여전히 여러 가지 방법으로 구현할 수 있습니다. `toupper` 함수는 C언어 표준 라이브러리에 포함되어 있지만, 아스키 값에 직접 접근하여 변환하는 등의 대안도 있습니다. 이 때는 'a'와 'z' ASCII 값 사이에 있는 문자만 변환하면 됩니다. 추가적으로 첫 글자만 대문자로 만드는 "Capitalize"이라는 다른 구현 방법도 있습니다.

## 참고자료 (See Also)
* [toupper 함수에 대한 cplusplus.com 문서](http://www.cplusplus.com/reference/cctype/toupper/)
* [C표준 라이브러리에 대한 Wikipedia 글](https://en.wikipedia.org/wiki/C_standard_library)