---
title:                "C: 텍스트 검색 및 대체하기"
simple_title:         "텍스트 검색 및 대체하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜?

텍스트를 찾고 바꾸는 것에 대한 *왜*를 간략히 설명합니다.

텍스트를 찾고 바꾸는 것은 프로그래밍에서 매우 일반적인 작업입니다. 예를 들어, 특정 단어를 다른 단어로 바꾸는 것은 매우 유용합니다. 또는 오타를 수정하거나 특정 패턴을 찾아서 바꾸는 것도 텍스트를 이해하는데 도움이 됩니다.

## 어떻게?

아래의 "```C ...```" 코드 블록에서 코딩 예제와 샘플 출력을 확인할 수 있습니다. 

```C
#include <stdio.h>

int main()
{
    // 텍스트를 찾고 바꾸는 예제
    char str[100] = "안녕하세요! 오싹한 여름입니다.";
    char find[10] = "오싹한";
    char replace[10] = "시원한";
    char *ptr;

    ptr = strstr(str, find); // 특정 패턴을 찾는 함수
    strncpy(ptr, replace, strlen(replace)); // 특정 패턴을 다른 패턴으로 치환하는 함수
    printf("%s", str); // 출력: 안녕하세요! 시원한 여름입니다.

    return 0;
}
```

## 더 깊이 파고들기

텍스트를 찾고 바꾸는 기법에 대해 더 자세히 알아보겠습니다. 

- C 언어에서는 `strstr()` 함수를 사용하여 특정 패턴을 찾을 수 있습니다. 이 함수는 첫 번째 인자인 문자열에서 두 번째 인자인 패턴을 찾아서 해당 위치의 포인터를 반환합니다. 만약 찾지 못하면 `NULL`을 반환합니다.
- `strncpy()` 함수는 특정 위치에서 시작하여 지정된 길이만큼의 문자열을 다른 문자열로 복사합니다. 위의 예제에서는 특정 패턴을 다른 패턴으로 치환하기 위해 이 함수를 사용했습니다.
- 위의 예제에서는 오타를 수정하는 것이 아니라 더 일반적인 목적인 텍스트를 찾아서 바꾸는 것을 기준으로 설명했습니다. 따라서 특정 문자열에 오타가 있는 경우에는 `strncpy()` 함수를 사용해서 수정할 수도 있습니다.

## 더 보기

- [C 언어에서 문자열 다루는 함수들](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [strstr() 함수에 대한 C 레퍼런스 페이지](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [strncpy() 함수에 대한 C 레퍼런스 페이지](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)
- [C 언어 공식 문서](https://www.iso.org/standard/74528.html)

# 참고하기