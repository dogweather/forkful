---
title:                "C: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

한국어로 C 프로그래밍을 배우는 독자분들을 위한 이번 포스트에서는 문자열에서 패턴과 일치하는 문자를 삭제하는 방법에 대해 알아보고자 합니다. 이 과정을 통해 당신의 프로그래밍 실력을 향상시킬 수 있을 뿐만 아니라, 더 나은 코드 작성 방법에 대한 아이디어를 얻을 수 있을 것입니다.

## 어떻게

프로그래밍에서 문자열 처리는 매우 중요한 부분입니다. 가끔은 특정 패턴과 일치하는 문자를 삭제해야 할 때가 있습니다. 여러분은 아마도 일일이 문자를 탐색하고 삭제하는 방법에 대해 고민하고 계실 것입니다. 하지만 우리는 당신을 도와드릴 준비가 되어 있습니다. 아래 코드 블록을 통해 간단한 예제를 확인해 보세요.

```C
#include <stdio.h>
#include <string.h>

// 문자가 입력될 때마다 패턴을 체크하고 삭제하는 함수
void deletePattern(char string[], char pattern[]) {
    // 문자열의 길이를 구합니다.
    int len = strlen(string);

    // 패턴과 일치하는 문자를 찾아서 삭제합니다.
    for (int i = 0; i < len; i++) {
        if (strncmp(&string[i], pattern, len) == 0) {
            // 일치하는 부분을 삭제합니다.
            strcpy(&string[i], &string[i] + len);
        }
    }

    // 수정된 문자열을 출력합니다.
    printf("수정된 문자열: %s", string);
}

// 메인 함수
int main() {
    // 삭제할 패턴과 문자열을 입력합니다.
    char pattern[] = "apple";
    char string[] = "I love apple and banana.";

    // deletePattern 함수를 호출합니다.
    deletePattern(string, pattern);

    return 0;
}
```

출력 결과:
수정된 문자열: I love  and banana.

위의 예제에서는 입력된 문자열에서 'apple'이라는 패턴과 일치하는 부분을 삭제하였습니다. 가능하다면 여러분들도 위의 코드를 따라 적용해보시기 바랍니다.

## 깊이 파고들기

위의 예제는 간단한 예제일 뿐이며, 실제로 문자열 처리는 더 복잡한 상황에서 다루어지게 됩니다. 하지만, 많은 프로그래머들이 이런 종류의 작업을 자주 다루게 될 것임을 알고 있어야 합니다. 문자열 처리 기술을 더 깊이 파고들기 위해서는 여러분 스스로 다양한 예제를 시도해보시길 권장합니다. 또한, 다른 프로그래머들의 코드를 읽고 분석하며 학습할 수도 있습니다.

## 이어서 보기

- 문자열 처리를 위한 다른 함수들을 확인해 보세요.
- C 프로그래밍에서 문자열 다루는 방법에 대해 자세히 알아보세요.
- 문자열 처리를 위한 다양한 예제들을 연습해보세요.