---
title:                "정규 표현식을 사용하는 방법"
html_title:           "C: 정규 표현식을 사용하는 방법"
simple_title:         "정규 표현식을 사용하는 방법"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?
정규식을 사용한다는 것은 정규 표현식으로 텍스트를 검색하고 조작하는 것을 말합니다. 프로그래머들은 이를 사용하는 이유는 문자열의 패턴을 파악하고 조작하는 데 용이하기 때문입니다.

## 방법:
C 언어에서 정규식을 사용하는 방법은 간단합니다. 먼저, `<regex.h>` 라이브러리를 include 해줍니다. 그리고 `regex_t` 구조체를 정의하고 `regcomp()` 함수를 사용하여 정규식을 컴파일합니다. 컴파일된 정규식은 `regexec()` 함수를 사용하여 텍스트에서 검색할 수 있습니다. 아래는 간단한 예제 코드와 출력 결과입니다:

```c
#include <stdio.h>
#include <regex.h>

int main(){
    regex_t regex;
    int result;
    char text[] = "Hello, world!";

    // 컴파일된 정규식 패턴: Hello 로 시작하는 문자열을 검색
    result = regcomp(&regex, "^Hello", 0);

    // 텍스트에서 정규식 검색
    result = regexec(&regex, text, 0, NULL, 0);

    if (result == 0){
        // 정규식에 일치하는 문자열을 찾았을 경우
        printf("정규식과 일치하는 문자열을 찾았습니다!\n");
    } else {
        // 일치하는 문자열이 없을 경우
        printf("일치하는 문자열이 없습니다.\n");
    }

    return 0;
}
```

출력 결과:
```
정규식과 일치하는 문자열을 찾았습니다!
```

## 깊이 파고들기:
정규식은 1950년대에 활용하기 위해 개발된 형식 언어입니다. C 언어에서는 다른 라이브러리를 사용하여 정규식을 처리할 수도 있습니다. 예를 들어, `<pcre.h>` 라이브러리를 활용하여 더 다양한 정규식 문법을 처리할 수 있습니다. 또한, 정규식을 이용하여 문자열의 패턴 뿐만 아니라 대체, 반복 등의 더 복잡한 작업을 수행할 수도 있습니다.

## 더 알아보기:
- [C 언어로 정규식 사용하기](https://www.regular-expressions.info/c.html)
- [정규식 패턴 테스트하기](https://regexr.com/)
- [정규식 실습 사이트](https://regexone.com/)