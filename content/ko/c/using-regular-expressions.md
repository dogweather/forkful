---
title:                "C: 일반 표현식 사용하기"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜 정규 표현식을 사용해야 할까요?

정규 표현식은 C 프로그래머들에게 매우 유용한 도구입니다. 이를 사용하면 문자열에서 원하는 패턴을 쉽게 찾을 수 있으며, 코드를 줄이고 가독성을 높여줍니다. 또한 많은 언어와 프로그램에서도 지원되는 범용적인 도구로서, 다양한 상황에서 유용하게 사용할 수 있습니다.

## 사용 방법

정규 표현식을 사용하기 위해서는 `regex.h` 라이브러리를 불러와야 합니다. 이후 `regex_t` 구조체와 `regmatch_t` 구조체를 선언해줍니다. 그리고 `regcomp()` 함수를 이용하여 패턴을 컴파일하고, `regexec()` 함수를 이용하여 문자열에서 패턴을 검색할 수 있습니다. 예제 코드를 통해 쉽게 이해해봅시다.

```C
#include <stdio.h>
#include <regex.h>

int main() {
    regex_t regex; // regex_t 구조체 선언
    regmatch_t match[1]; // regmatch_t 구조체 선언. 여러 패턴을 찾을 때는 배열로 선언.

    char *str = "My email is abc@def.com"; // 검색할 문자열

    // 패턴 컴파일
    regcomp(&regex, "[a-z]+@[a-z]+\\.com", 0); 

    // 문자열에서 패턴 검색
    regexec(&regex, str, 1, match, 0); 

    // 패턴에 일치하는 결과 출력
    printf("%.*s\n", (int)(match[0].rm_eo - match[0].rm_so), &str[match[0].rm_so]); 
    // 출력 결과: abc@def.com

    // regex 구조체 메모리 해제
    regfree(&regex); 

    return 0;
}
```

위 예제 코드에서 사용한 패턴은 이메일 주소를 검색하는 것이며, `[a-z]+@[a-z]+\\.com`와 같이 정규 표현식으로 표현됩니다. 이 부분을 원하는 패턴으로 수정하여 사용하면 됩니다.

## 더 깊게 들어가보기

정규 표현식에는 다양한 특수 문자가 존재하며, 각각의 의미와 사용법을 알고 있어야 더욱 효율적으로 사용할 수 있습니다. 예를 들어, `.`는 문자 하나를 나타내는 특수 문자입니다. 이를 이용하여 `ba.`는 `bag`, `bad`, `ban`과 같이 마지막에 `a`가 있는 단어를 찾을 수 있습니다. 또한 `*`는 `*`앞의 문자가 0번 이상 반복됨을 나타내며, `+`는 1번 이상 반복됨을 나타냅니다.

또한, `|`을 이용하면 여러 패턴 중 하나에 일치하는 문자열을 찾을 수 있습니다. 예를 들어, `ba|ca`는 `ba` 또는 `ca`에 일치하는 문자열을 찾을 수 있습니다.

정규 표현식의 특수 문자 중에서도 `()`와 `[]`는 매우 유용하게 사용됩니다. `()`는 정규 표현식을 하나의 그룹으로 묶어주는 역할을 하며, `[]`는 그 안에 들어있는 문자 중 하나와 일치하는 것을 찾을 수 있습니다. 예를 들어, `[a-d]`는 `a`, `b`, `c`, `d` 중 하나와 일치