---
title:                "정규 표현식 활용하기"
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜 사용하는가?)
정규 표현식은 문자열 검색, 추출, 대체를 간단히 하는 패턴이다. 프로그래머는 코드를 짧고, 빠르며, 유연하게 작성하기 위해 사용한다.

## How to (사용 방법):
C에서 정규 표현식을 사용하기 위해서는 `regex.h` 헤더 파일을 포함시켜야 한다. 아래 예제는 정규 표현식을 사용해서 이메일 형식을 찾는 방법을 보여준다.
```C
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main() {
    regex_t regex;
    int ret;
    ret = regcomp(&regex, "[[:alnum:]]+@[[:alnum:]]+\\.[a-zA-Z]{2,}", 0);
    if (ret) { exit(1); } // 컴파일 실패 처리

    const char *test_email = "user@example.com";
    ret = regexec(&regex, test_email, 0, NULL, 0);

    if (!ret) {
        printf("\"%s\" is a valid email.\n", test_email);
    } else {
        printf("\"%s\" is not a valid email.\n", test_email);
    }

    // 메모리 해제
    regfree(&regex);

    return 0;
}
```
출력:
```
"user@example.com" is a valid email.
```

## Deep Dive (심층 분석):
정규 표현식은 1950년대에 시작되어, 1980년대에 Perl 언어에 도입됐고, 많은 언어가 따라했다. C언어에서 `regex.h`는 POSIX 표준에 따른다. `regcomp`는 패턴을 컴파일하고, `regexec`는 비교를 실행한다. `grep`, `sed`, `awk` 등의 대체 방법도 있다. 하지만 정규 표현식은 복잡한 문자열 작업에 탁월하다.

## See Also (추가 정보):
- [GNU C Library: Regular Expressions](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [POSIX Regular Expressions](https://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap09.html)
- [Learn Regex The Hard Way](https://regex.learncodethehardway.org/book/)
