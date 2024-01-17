---
title:                "텍스트 검색 및 교체"
html_title:           "C: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 어떤 것인가 & 왜?: 
텍스트를 검색하고 바꾸는 것은 프로그래머들이 자주 하는 일입니다. 이 과정은 특정 텍스트를 찾아서 다른 텍스트로 대체하는 것을 뜻합니다. 이는 코드를 간편하게 수정하고 유지보수하는 데 도움이 됩니다.

## 방법: 
프로그래밍에서 텍스트를 검색하고 바꾸는 방법은 다양합니다. 가장 간단한 방법은 ```C ... ``` 코드 블록 내에 `strstr()` 함수를 사용하는 것입니다. 예를 들어, 다음과 같이 사용할 수 있습니다:

```
#include <stdio.h>
#include <string.h>

int main() {
  char text[] = "Hello world";
  char target[] = "world";
  char replacement[] = "universe";

  char *result = strstr(text, target);

  if (result) {
    // target을 찾으면 대체
    strncpy(result, replacement, strlen(replacement));
  }

  printf("%s", text);

  return 0;
}

// 출력: Hello universe
```

## 깊게 들어가기: 
텍스트를 검색하고 바꾸는 방법은 다양하지만 가장 간단한 방법은 ```strstr()``` 함수를 사용하는 것입니다. 이 함수는 C 언어에서 문자열을 검색하는 데 사용되는 함수입니다. 코드를 작성할 때 다른 방법으로도 텍스트를 검색하고 바꾸는 법을 고려해 볼 수 있습니다. 또한 텍스트 검색 및 대체는 많은 컴퓨터 프로그램에서 사용되는 중요한 기능이며 항상 업그레이드되고 개선되고 있습니다.

## 참고 자료: 
- [C strstr() 함수로 문자열 검색](https://www.geeksforgeeks.org/strstr-in-ccpp/)
- [프로그래밍에서 문자열 검색 및 대체하기](https://www.quora.com/How-do-I-replace-text-in-C-programming-language)
- [텍스트 검색 및 대체를 위한 다양한 알고리즘](https://www.guru99.com/text-search-replace-algorithms.html)