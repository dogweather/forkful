---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

문자열에서 부분 문자열을 추출하는 것은 그 문자열의 특정 부분을 가져오는 것입니다. 이는 데이터를 분석하고 파싱할 때 매우 중요합니다.

## 어떻게 하는가 :

'C'언어에서 문자열에서 부분 문자열을 추출하는 방법을 설명하겠습니다. 

```C
#include <string.h> 
#include <stdio.h> 

int main() 
{ 
    char input_string[20] = "Hello, World!";
    char substring[20];
    strncpy(substring, &input_string[7], 5);
    substring[5] = '\0';
    printf("%s", substring);
    return 0; 
} 
```
위 코드의 실행 결과는 "World"입니다.

## 깊게 알아보기

부분 문자열 추출은 문자열 다루기의 기초적인 부분이며, 이는 'C'언어가 만들어진 1972년부터 소프트웨어 개발에서 필수적인 요소가 되어왔습니다. `strncpy` 함수는 현재 널-종료 문자열에서 부분 문자열을 추출하는 가장 일반적인 방법입니다.

다른 옵션으로는 `sscanf`, `strtok` 등의 함수도 있지만 그 사용법은 `strncpy`보다 복잡할 수 있습니다.

이러한 함수들이 어떻게 작동하는지를 이해하는 것은 메모리 관리와 형 변환에 대한 깊은 이해를 요구합니다. 특히, `strncpy`함수는 복사하려는 문자열의 길이보다 목적지 버퍼가 작다면 버퍼 오버플로우를 일으키는 버그가 발생할 수 있습니다. 따라서, 안전하게 코드를 작성하기 위해서는 이러한 부분들을 반드시 고려해야 합니다.

## 참고 자료

* 부분 문자열 추출을 활용한 더 복잡한 예제와 응용을 원한다면, 다음 링크를 참조해 주세요: [C Programming/String manipulation](https://en.wikibooks.org/wiki/C_Programming/String_manipulation) 
* 부분 문자열 추출에 관련된 다른 함수들에 대해 알고 싶다면, 다음을 참조해 주세요: [C string handling](https://en.wikipedia.org/wiki/C_string_handling)
* 메모리 관리에 대해 더 깊게 알아보려면, 다음 링크를 참조하십시오: [C dynamic memory allocation](https://en.wikipedia.org/wiki/C_dynamic_memory_allocation)