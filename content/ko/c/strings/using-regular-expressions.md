---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:08.458161-07:00
description: "\uC815\uADDC \uD45C\uD604\uC2DD(regex)\uC740 \uC815\uC758\uB41C \uD328\
  \uD134\uC744 \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\uC744 \uAC80\uC0C9, \uB9E4\
  \uCE6D, \uC870\uC791\uD558\uB294 \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC785\uB825\uAC12\uC758 \uC720\uD6A8\
  \uC131 \uAC80\uC0AC, \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130 \uD30C\uC2F1, \uD070\
  \ \uD14D\uC2A4\uD2B8 \uD30C\uC77C \uB0B4\uC758 \uD328\uD134 \uCC3E\uAE30\uC640 \uAC19\
  \uC740 \uC791\uC5C5\uC744 \uC704\uD574 \uC815\uADDC \uD45C\uD604\uC2DD\uC744 \uAD11\
  \uBC94\uC704\uD558\uAC8C \uC0AC\uC6A9\uD558\uBA70, \uC774\uB85C \uC778\uD574 C \uC5B8\
  \uC5B4\uB97C \uD3EC\uD568\uD55C\u2026"
lastmod: 2024-02-19 22:05:14.820222
model: gpt-4-0125-preview
summary: "\uC815\uADDC \uD45C\uD604\uC2DD(regex)\uC740 \uC815\uC758\uB41C \uD328\uD134\
  \uC744 \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\uC744 \uAC80\uC0C9, \uB9E4\uCE6D\
  , \uC870\uC791\uD558\uB294 \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC785\uB825\uAC12\uC758 \uC720\uD6A8\uC131\
  \ \uAC80\uC0AC, \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130 \uD30C\uC2F1, \uD070 \uD14D\
  \uC2A4\uD2B8 \uD30C\uC77C \uB0B4\uC758 \uD328\uD134 \uCC3E\uAE30\uC640 \uAC19\uC740\
  \ \uC791\uC5C5\uC744 \uC704\uD574 \uC815\uADDC \uD45C\uD604\uC2DD\uC744 \uAD11\uBC94\
  \uC704\uD558\uAC8C \uC0AC\uC6A9\uD558\uBA70, \uC774\uB85C \uC778\uD574 C \uC5B8\uC5B4\
  \uB97C \uD3EC\uD568\uD55C\u2026"
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜 사용하는가?

정규 표현식(regex)은 정의된 패턴을 사용하여 문자열을 검색, 매칭, 조작하는 방법을 제공합니다. 프로그래머들은 입력값의 유효성 검사, 텍스트 데이터 파싱, 큰 텍스트 파일 내의 패턴 찾기와 같은 작업을 위해 정규 표현식을 광범위하게 사용하며, 이로 인해 C 언어를 포함한 모든 언어에서 강력한 도구가 됩니다.

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
