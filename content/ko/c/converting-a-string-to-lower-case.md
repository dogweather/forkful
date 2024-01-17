---
title:                "문자열을 소문자로 변환하기"
html_title:           "C: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜? 
문자열을 소문자로 변환하는 것은 프로그래머가 문자열을 간단하게 처리하고 비교할 수 있도록 하기 위한 방법입니다. 예를 들어, 문자열 "HELLO"를 모두 대문자로 쓸 때와 소문자로 쓸 때를 다루는 두 가지 다른 기능을 구현해야 할 때 매우 유용합니다.

## 방법: 
```C
#include <stdio.h>
#include <ctype.h>

int main()
{
  char str[] = "Hello, World!";
  
  // lowercase conversion using tolower() function
  int i = 0;
  while (str[i])
  {
    str[i] = tolower(str[i]);
    i++;
  }
  
  printf("%s", str);
  
  return 0;
}
```
출력: hello, world!

## 깊이 파고들기: 
문자열을 소문자로 변환하는 아이디어는 프로그램 언어마다 다르지만, 대부분의 프로그래밍 언어에서 문자열 처리를 위한 내장 함수를 제공합니다. C에서는 tolower() 함수를 사용해 문자를 소문자로 변환할 수 있고, 문자가 대문자인지 소문자인지 확인하는 isupper() 함수를 제공합니다. 또한 문자열을 복사하고 비교하는 등 다양한 방법으로 문자열을 대소문자 구분 없이 처리할 수 있습니다.

## 관련 자료: 
- [C 문자열 함수 - tolower()](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [C 문자열 함수 - isupper()](https://www.tutorialspoint.com/c_standard_library/c_function_isupper.htm)
- [Ambitious Coder - 문자열에 대소문자 변환 기능 구현하기](https://www.ambitiouscoder.com/c-string-case-conversion/)