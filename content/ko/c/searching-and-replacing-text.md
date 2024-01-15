---
title:                "텍스트 검색 및 대체"
html_title:           "C: 텍스트 검색 및 대체"
simple_title:         "텍스트 검색 및 대체"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

프로그래밍에서 텍스트를 찾고 바꾸는 작업은 소프트웨어 개발에서 매우 중요한 부분입니다. 그것이 때문에 당신이 프로그래머라면 효율적으로 텍스트를 검색하고 수정하는 방법을 배워야합니다.

## 어떻게

텍스트를 검색하고 바꾸는 방법은 C의 내장 함수인 `strchr()`과 `strstr()`을 사용하는 것입니다. 예를 들어, 다음 코드는 문자열에서 `hello`라는 단어를 `hi`로 바꿀 것입니다.

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str[] = "Hello world";
    char *ptr = strstr(str, "Hello"); // str에서 "Hello" 문자열을 찾습니다.
    strncpy(ptr, "Hi", 2); // "Hello"를 "Hi"로 바꿉니다.

    printf("%s", str); // "Hi world"를 출력할 것입니다.

    return 0;
}
```

또는 `strtok()` 함수를 사용하여 문자열을 분할하고 원하는 부분을 찾고 바꿀 수도 있습니다. 위의 예제에서 `strtok()`을 사용하여 단어를 분할하고 `strcmp()` 함수를 이용하여 단어를 검색하고 바꿀 수 있습니다.

## 깊게 파고들기

텍스트를 더욱 복잡하게 검색하고 바꾸는 방법도 있습니다. 예를 들어, 정규 표현식을 사용하면 패턴을 설정하여 텍스트를 검색하고 바꿀 수 있습니다. 또한 사용자 정의 함수를 작성하여 원하는 방식대로 텍스트를 수정할 수도 있습니다.

## 관련 링크

- [텍스트 검색과 바꾸기 관련 C 함수들](https://www.geeksforgeeks.org/c-programming-language/)
- [정규 표현식에 대한 자세한 내용](https://www.regular-expressions.info/)
- [텍스트 처리를 위한 사용자 정의 함수 예제](https://www.codeproject.com/Articles/1088/Flexible-C-1-Pattern-Substitution)