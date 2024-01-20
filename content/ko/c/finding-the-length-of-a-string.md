---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜? 

문자열의 길이를 찾는 것은 문자가 얼마나 많이 연결되어 있는지를 확인하는 것입니다. 프로그래머들은 메모리 최적화, 데이터 유효성 검사, 상황에 따른 적절한 출력 등을 위해 이를 실행합니다.

## 어떻게:

### C 언어를 사용하여 문자열의 길이 찾기:

```C 
#include<stdio.h> 
#include<string.h> 

int main() 
{ 
   char str[] = "Code it Better!"; 
   int leng = strlen(str); 
   
   printf("Length of string = %d", leng); 
   
   return 0; 
} 
```
### 출력 :

```C
Length of string = 15
```
## 딥다이브:

### 히스토리컬 컨텍스트:
사실상 모든 프로그래밍 언어는 문자열 길이를 찾는 함수를 제공합니다. 이는 자료구조를 다루는 기본적인 요소이므로, 가장 초기의 프로그래밍 언어부터 도입되었습니다.

### 대안 :
"strlen" 함수 외에도  "for" 루프를 활용해서도 문자열의 길이를 찾을 수 있습니다. 그러나 이 방법은 코드를 불필요하게 복잡하게 만들 수 있습니다.

### 구현 세부정보: 
C 언어에서 "strlen" 함수는 문자열의 마지막에 자동으로 추가되는 널 문자 ('\0')를 찾아 문자열의 길이를 계산합니다.

## 추가 정보 :

1. [C 언어 문자열 관련 함수 설명서](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
2. [C 프로그래밍에서 문자열 처리에 대한 자세한 이해](https://www.geeksforgeeks.org/string-in-c/)