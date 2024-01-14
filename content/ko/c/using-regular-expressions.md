---
title:                "C: 정규식 사용하기"
simple_title:         "정규식 사용하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

왜 누군가 정규 표현식을 사용해야 하는지는 간단합니다. 정규 표현식은 문자열 내에서 특정 패턴을 찾아내기 위해 사용되는 강력한 도구입니다. 이는 코드에서 문자열을 처리하거나 특정 값만을 선택하기 위해 많이 사용됩니다.

## 사용 방법

먼저 정규 표현식을 사용하기 위해서는 헤더 파일 `<regex.h>`를 포함해야 합니다. 그런 다음, 정규 표현식과 비교할 문자열을 `char` 배열로 선언합니다. 그리고 `regex_t` 구조체를 생성하여 정규 표현식을 저장하고, `regcomp` 함수를 사용하여 정규 표현식을 컴파일합니다.

```C
#include <stdio.h>
#include <regex.h>

int main(void) {
    int ret;
    regex_t regex;
    char str[] = "Hello, world!";
    
    ret = regcomp(&regex, "world", 0);
    if (ret) {
        printf("정규 표현식 컴파일 실패\n");
        return 1;
    }
    
    ret = regexec(&regex, str, 0, NULL, 0);
    if (!ret) {
        printf("'world' 발견!\n");
    } else if (ret == REG_NOMATCH) {
        printf("'world'를 찾지 못했습니다.\n");
    }
    
    regfree(&regex);
    
    return 0;
}
```

위 예시 코드에서는 문자열 `str`에서 정규 표현식 `world`을 찾는 것을 목적으로 합니다. `regcomp` 함수에서는 정규 표현식을 컴파일하고, `regexec` 함수에서는 문자열에 해당 정규 표현식이 있는지 검사합니다. 마지막으로 `regfree` 함수를 사용하여 `regex_t` 구조체를 해제합니다.

## 깊이 파고들기

정규 표현식은 매우 강력한 도구이지만, 그 만큼 복잡하고 어려운 개념입니다. 다양한 메타 문자와 그 사용법, 그리고 여러 예외 상황들을 고려해야 하기 때문입니다. 또한 정규 표현식을 사용할 때에는 최적화에 주의해야 합니다. 간단한 정규 표현식일지라도 문자열의 길이나 패턴의 수에 따라 실행 속도가 크게 달라질 수 있으므로, 적절한 최적화 방법을 찾는 것이 중요합니다.

## 관련 링크들

[Learn C.org - 정규 표현식](https://www.learn-c.org/en/Regular_Expressions)  
[C Reference - 정규 표현식 관련 함수](https://en.cppreference.com/w/c/regex)  
[정규 표현식 테스트 도구](https://regexr.com/)