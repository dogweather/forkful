---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:08.458161-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: C\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\uC2DD\
  \uC744 \uC0AC\uC6A9\uD558\uAE30 \uC704\uD574 \uC8FC\uB85C POSIX \uC815\uADDC \uD45C\
  \uD604\uC2DD \uB77C\uC774\uBE0C\uB7EC\uB9AC(`<regex.h>`)\uB97C \uC791\uC5C5\uD558\
  \uAC8C \uB429\uB2C8\uB2E4. \uC774 \uC608\uC2DC\uB294 \uAE30\uBCF8 \uD328\uD134 \uB9E4\
  \uCE6D\uC744 \uBCF4\uC5EC\uC90D\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.905895-06:00'
model: gpt-4-0125-preview
summary: "C\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\uC2DD\uC744 \uC0AC\uC6A9\uD558\uAE30\
  \ \uC704\uD574 \uC8FC\uB85C POSIX \uC815\uADDC \uD45C\uD604\uC2DD \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC(`<regex.h>`)\uB97C \uC791\uC5C5\uD558\uAC8C \uB429\uB2C8\uB2E4."
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
weight: 11
---

## 사용 방법:
C에서 정규 표현식을 사용하기 위해 주로 POSIX 정규 표현식 라이브러리(`<regex.h>`)를 작업하게 됩니다. 이 예시는 기본 패턴 매칭을 보여줍니다:

```c
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main(){
    regex_t regex;
    int return_value;
    char *pattern = "^a[[:alnum:]]"; // 'a'로 시작하고 알파벳 또는 숫자가 따르는 문자열과 매칭되는 패턴
    char *test_string = "apple123";

    // 정규 표현식 컴파일
    return_value = regcomp(&regex, pattern, REG_EXTENDED);
    if (return_value) {
        printf("정규 표현식 컴파일 실패\n");
        exit(1);
    }

    // 정규 표현식 실행
    return_value = regexec(&regex, test_string, 0, NULL, 0);
    if (!return_value) {
        printf("매칭됨\n");
    } else if (return_value == REG_NOMATCH) {
        printf("매칭되지 않음\n");
    } else {
        printf("정규 표현식 매칭 실패\n");
        exit(1);
    }

    // 정규 표현식에서 사용된 메모리 해제
    regfree(&regex);

    return 0;
}
```

매칭되는 문자열("apple123")에 대한 샘플 출력:
```
매칭됨
```
매칭되지 않는 문자열("banana")에 대해:
```
매칭되지 않음
```

## 심층 분석:
POSIX 표준의 일부인 C에서의 정규 표현식은 문자열 매칭 및 조작을 수행하는 견고한 방법을 제공합니다. 하지만, C의 POSIX 정규 표현식 라이브러리 API는 Python 또는 Perl 같이 일급 문자열 조작 기능이 있는 언어에서 찾아볼 수 있는 것보다 더 번거롭게 여겨집니다. 패턴의 문법은 언어 간에 유사하지만, C는 정규 표현식 패턴을 준비, 실행, 정리하는 데 더 많은 보일러플레이트 코드와 수동 메모리 관리를 요구합니다.

이러한 도전에도 불구하고, C에서 정규 표현식을 사용하는 방법을 배우는 것은 낮은 수준의 프로그래밍 개념에 대한 이해를 깊게 하며 보람이 있습니다. 추가적으로, 텍스트 처리 및 데이터 추출과 같은 분야에서 정규 표현식이 없어서는 안 될 도구이기 때문에 C 프로그래밍에 대한 가능성을 엽니다. 더 복잡한 패턴이나 정규 표현식 작업에 대해서는 PCRE(Perl 호환 정규 표현식) 라이브러리와 같은 대안이 더 많은 기능과 다소 쉬운 인터페이스를 제공할 수 있지만, C 프로젝트에 외부 라이브러리를 통합해야 합니다.
