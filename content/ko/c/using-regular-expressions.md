---
title:    "C: 정규식을 사용하는 법"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜 정규식을 사용해야 할까요?

C 프로그래밍에서 정규식은 매우 유용한 도구입니다. 정규식을 사용하면 검색 및 대체 기능을 포함하여 문자열을 처리하는 데 매우 유용합니다. 이것은 특히 데이터를 다루거나 패턴을 추출해야 할 때 유용합니다.

## 사용 방법

정규식을 사용하기 위해 여러분은 <regex.h> 헤더 파일을 인클루드해야 합니다. 그리고 다음과 같은 코드를 사용하여 정규식을 컴파일하고 문자열과 비교할 수 있습니다.

```C
#include <stdio.h>
#include <regex.h>

int main() {
    // 정규식을 컴파일합니다.
    regex_t regex;
    int ret = regcomp(&regex, "Hello [Ww]orld", 0);
    if (ret != 0) {
        // 정규식 컴파일 중 오류가 발생했습니다.
        printf("정규식 컴파일 오류!\n");
        return 1;
    }

    // 문자열을 검색합니다.
    char* text = "Hello World";
    ret = regexec(&regex, text, 0, NULL, 0);
    if (ret == 0) {
        printf("'%s'는 정규식과 일치합니다.\n", text);
    } else {
        printf("'%s'는 정규식과 일치하지 않습니다.\n", text);
    }

    // 정규식 객체를 해제합니다.
    regfree(&regex);

    return 0;
}
```

위 코드의 결과는 다음과 같습니다.

```
'Hello World'는 정규식과 일치합니다.
```

위에서 사용된 정규식은 "Hello World"라는 문자열과 일치하므로 출력 결과가 "정규식과 일치합니다."가 됩니다.

## 깊이 들어가기

정규식을 사용하는 데에는 많은 내용이 있지만 여기서는 단순히 기본적인 사용 방법만을 다뤄보았습니다. 정규식은 패턴 매칭, 대체 및 추출에 유용하지만 패턴을 작성하는 것은 쉽지 않을 수 있습니다. 따라서 정규식을 배울 때에는 많은 연습과 이해가 필요합니다.

## 참고 자료

- [GladysKnight/cse320-midterm-lecture8-regex.c](https://github.com/GladysKnight/cse320-midterm-lecture8-regex.c): C 언어로 정규식을 사용하는 예제 코드입니다.
- [GeeksforGeeks: Regular Expressions in C](https://www.geeksforgeeks.org/regular-expressions-in-c/): C 언어에서 정규식을 사용하는 방법에 대한 자세한 설명을 제공합니다.
- [CReference: <regex.h> - Regular expression handling](https://en.cppreference.com/w/c/regex): C 언어에서 정규식을 다루는 데 필요한 헤더 파일 <regex.h>에 대한 참고 문서입니다.