---
title:                "정규 표현식 사용하기"
html_title:           "C: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜
정규 표현식을 사용하는 이유는 텍스트 데이터에서 원하는 패턴을 찾거나 변환하기 위해서입니다.

## 방법
정규 표현식을 사용하는 방법은 다양하게 있지만, 여기에는 가장 기본적인 패턴 매칭과 치환 방법을 알려드리겠습니다.

### 패턴 매칭
패턴 매칭 코드의 기본 형식은 다음과 같습니다.

```C
#include <stdio.h>
#include <regex.h>

int main()
{
    regex_t regex;
    int ret;

    // 정규 표현식 컴파일
    ret = regcomp(&regex, "패턴", 0);

    // 패턴 매칭
    ret = regexec(&regex, "문자열", 0, NULL, 0);

    // 매칭된 문자열 출력
    printf("%s\n", "문자열");

    // 정규 표현식 메모리 해제
    regfree(&regex);

    return 0;
}
```

위 코드를 실행하면 "문자열"이 매칭되고 출력됩니다. 패턴과 매칭할 문자열을 바꿔가며 여러 가지 경우를 테스트해보세요!

### 치환
패턴 매칭과 비슷하지만, 치환을 수행하기 위한 추가적인 매개변수가 필요합니다.

```C
#include <stdio.h>
#include <regex.h>

int main()
{
    regex_t regex;
    int ret;
    char *result;

    // 정규 표현식 컴파일
    ret = regcomp(&regex, "패턴", 0);

    // 치환
    result = regsub("문자열", "치환할 문자열", &regex, 0);

    // 치환된 문자열 출력
    printf("%s\n", result);

    // 메모리 해제
    free(result);
    regfree(&regex);

    return 0;
}
```

위 코드를 실행하면 "치환할 문자열"이 "문자열"로 치환되고 그 결과가 출력됩니다. 이러한 방식으로 정규 표현식을 사용하여 텍스트 데이터를 효과적으로 변환할 수 있습니다.

## 더 깊이 들어가기
여러 가지 패턴 매칭 기법과 고급 사용법에 대해 알아보세요. 정규 표현식은 텍스트 작업에서 매우 유용하며, 더 깊이 공부한다면 더 많은 기능을 발견할 수 있을 것입니다.

## 참고 자료
- [C 언어로 정규 표현식 다루기](https://bulljo.tistory.com/3)
- [정규 표현식 기초부터 활용까지](https://thebook.io/006723/ch09/)
- [정규식 테스트 사이트](https://regexr.com/)