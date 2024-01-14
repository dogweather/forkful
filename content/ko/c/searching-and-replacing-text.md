---
title:    "C: 텍스트 검색과 대체"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

### 왜: 텍스트를 검색하고 대체하는 것에 참여하는 이유 
텍스트 검색 및 교체는 프로그래머에게 매우 중요한 작업이다. 이를테면, 일반적으로 문제를 해결하거나 코드를 개선하는데에 사용된다. 따라서 이 작업을 효과적으로 수행하는 방법을 배우는 것은 매우 중요하다.

### 하는 방법: C 코드 블록 내에 코딩 예제와 샘플 출력 
우리는 먼저 텍스트 검색 및 교체를 위해 가장 일반적으로 사용되는 두 가지 함수인 "strstr()"와 "strreplace()"를 살펴볼 것이다. 아래는 이 두 함수가 어떻게 작동하는지 보여주는 예제 코드이다.

```C
#include <stdio.h>
#include <string.h>

int main() {
  char str1[] = "Hello, world!";
  char str2[] = "world";
  char newStr[] = "everyone";

  char *ptr = strstr(str1, str2);
  if (ptr) {
    strcpy(ptr, newStr);
  }

  printf("%s", str1);
  return 0;
}
```

위 코드는 "strstr()" 함수를 사용하여 문자열에서 "world"를 검색하고, 만약 찾았다면 "strreplace()" 함수를 사용하여 "world"를 "everyone"으로 대체한다. 그리고 마지막으로 변경된 문자열을 출력한다.

출력결과: Hello, everyone!

### 심층 분석: 텍스트 검색과 교체의 깊은 이해 
텍스트 검색과 교체는 프로그래밍에서 매우 중요한 기술 중 하나이다. 하지만 실제로 이를 효과적으로 사용하기 위해서는 조금 더 깊은 이해가 필요하다. 예를 들어 왜 "strstr()" 함수에서 두 번째 인자로 전달된 문자열은 기존 문자열에 포함된 문자열이어야 하는지, 혹은 왜 "strcpy()" 함수를 사용하여 문자열을 복사하는지 등의 질문에 대한 답을 찾아보는 것이 중요하다.

마지막으로, 텍스트 검색 및 교체 기술을 사용하여 작성한 코드는 매우 유용한 도구가 될 수 있다. 하지만 이를 사용할 때에는 어떤 입력값을 받는지, 어떤 예외 상황을 처리할 수 있는지 등에 대한 고려가 필요하다.

### 관련 링크 
- [C Programming Tutorial - Searching and Replacing Strings](https://www.programiz.com/c-programming/library-function/string.h/strstr)
- [C String Functions](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [String Manipulation in C](https://www.geeksforgeeks.org/string-manipulations-in-c-with-examples/)