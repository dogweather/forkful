---
title:                "문자열의 길이 찾기"
html_title:           "C: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 것에 참여하는 이유는 매우 간단합니다. 그것은 프로그램에서 문자열을 다룰 수 있게 해주기 때문입니다.

## 하우 투

문자열의 길이를 찾는 것은 C 프로그래밍에서 매우 기본적이고 필수적인 작업입니다. 이를 위해서는 다음과 같은 단계를 따릅니다.

1. `strlen()` 함수를 이용하여 문자열의 길이를 반환합니다.

```
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hello World";
    int len = strlen(str); // 길이를 변수에 저장합니다.
    printf("문자열의 길이: %d", len);
    return 0;
}
```

2. 위의 코드를 실행하면 `Hello World` 문자열의 길이가 출력됩니다.

```
문자열의 길이: 11
```

## 딥 다이브

C 프로그래밍에서 문자열의 길이를 찾는 것은 매우 간단한 작업처럼 보이지만, 실제로는 많은 내부 작업이 수행됩니다. `strlen()` 함수는 문자열의 첫 번째 문자부터 순차적으로 탐색하면서 NULL 문자 `\0`을 만날 때까지 카운팅하게 됩니다. 따라서 문자열의 길이가 늘어날수록 작업 시간도 늘어날 수 있습니다.

## 봐주세요

- [C 언어 튜토리얼](https://www.programiz.com/c-programming)
- [문자열 처리 관련 함수들](https://modoocode.com/141)
- [NULL 문자에 대한 이해](https://www.geeksforgeeks.org/null-character-c/)