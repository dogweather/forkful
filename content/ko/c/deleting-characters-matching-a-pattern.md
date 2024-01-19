---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
패턴에 일치하는 문자 삭제는 문자열에서 특정 패턴의 문자를 제거하는 작업입니다. 이런 작업은 데이터 정제, 텍스트 분석 또는 문자열 formatting 때문에 프로그래머들이 필요로 합니다. 

## 어떻게 할까?
아래와 같이 C에서 문자열이 저장된 배열에서 패턴에 일치하는 문자의 첫 번째 인스턴스를 제거하는 방법입니다.

```C
#include <stdio.h>
#include <string.h>

void remove_char(char* str, char c) {
    char* pr = str, *pw = str;
    while (*pr) {
        *pw = *pr++;
        pw += (*pw != c);
    }
    *pw = '\0'; // Terminate the resulting string
}

int main() {
    char str[] = "Hello, World!";
    remove_char(str, 'o');
    printf("%s\n", str);  // 출력: "Hell, Wrld!"
    return 0;
}
```

## 깊게 알아보기
패턴에 일치하는 문자 삭제는 사실 문자열 조작의 가장 기본적인 형태임에도 불구하고, C에서는 상당히 귀찮은 작업일 수 있습니다. 이는 C 언어가 문자열에 대한 기본적인 조작을 지원하지 않기 때문이지요.

하지만 이러한 방식으로 문자 삭제는 문자열 조작을 주도하는 키 요소 중 하나입니다. C는 문자열을 쉽게 바꿀 수 없도록 설계되었으므로, 문자를 제거하려면 명시적으로 배열 요소를 복사하고 새로운 문자열을 만듭니다. 

비록 C에서는 이게 불편하세요만, 다른 언어에서는 이 과정이 간단합니다. 예를 들어 Python의 `replace()` 메소드를 사용하면 문자를 쉽게 삭제할 수 있습니다.

## 참고하기
- C Programming Language, 2nd Edition (https://www.amazon.com/Programming-Language-Brian-W-Kernighan/dp/0131103628)
- Introduction to C Programming (https://www.learn-c.org/)
- Naver D2 C Programming (https://d2.naver.com/helloworld/13)