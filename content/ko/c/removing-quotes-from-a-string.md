---
title:                "문자열에서 따옴표 제거하기"
date:                  2024-01-26T03:38:31.158905-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열에서 따옴표 제거하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

문자열에서 따옴표를 제거한다는 것은 문자열의 내용에 포함된 모든 따옴표—단일('')이든 이중("")이든—를 제거하는 것을 의미합니다. 프로그래머들은 입력 데이터를 정화하거나, 데이터를 추가 처리를 위해 준비하거나, 문자열을 구분하기 위해 따옴표를 사용하는 언어에서 파일 경로와 명령어를 다룰 때 구문 오류를 피하기 위해 이를 실행합니다.

## 방법:

다음은 C 함수로, 문자열에서 그 성가신 따옴표를 제거해 줍니다:

```c
#include <stdio.h>
#include <string.h>

void remove_quotes(char *str) {
    char *p_read = str, *p_write = str;
    while (*p_read) {
        if (*p_read != '"' && *p_read != '\'') {
            *p_write++ = *p_read;
        }
        p_read++;
    }
    *p_write = '\0';
}

int main() {
    char str[] = "He said, \"Hello, 'world'!\"";
    printf("Original: %s\n", str);
    remove_quotes(str);
    printf("Sanitized: %s\n", str);
    return 0;
}
```

샘플 출력:

```
Original: He said, "Hello, 'world'!"
Sanitized: He said, Hello, world!
```

## 심층 분석

문자열에서 따옴표를 제거하는 작업은 프로그래밍의 새벽부터 있었던 작업으로, 데이터 위생이 오류(예: SQL 인젝션 공격)을 피하는 것뿐만 아니라 따옴표를 제어 문자로 혼동할 수 있는 시스템에 안전하게 문자열을 전달하기 위해 여전히 핵심입니다.

역사적으로, 다양한 언어는 이 작업을 다르게 처리합니다—일부는 내장 함수(`strip` in Python 같은)를 가지고 있으며, 반면 C와 같은 다른 언어들은 개발자에게 저수준 제어를 제공하는 데 중점을 두어 수동 구현을 필요로 합니다.

대안으로는 `strpbrk`와 같은 라이브러리 함수를 사용하여 따옴표를 찾거나, PCRE와 같은 라이브러리를 사용하여 정규 표현식(보다 복잡한 패턴을 위해)을 사용하는 것이 있지만, 단순히 따옴표를 제거하기 위해서는 과도한 방법일 수 있습니다.

위의 구현은 문자열의 각 문자를 스캔하여 따옴표가 아닌 문자만 쓰기 포인터 위치에 복사합니다. 결과 문자열을 위한 추가 메모리가 필요 없기 때문에 효율적입니다.

## 참고자료

- [C 표준 라이브러리 함수](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Perl 호환 정규 표현식](https://www.pcre.org/)
- [C에서의 포인터 이해하기](https://www.learn-c.org/en/Pointers)
- [C에서의 안전한 코딩](https://owasp.org/www-project-secure-coding-in-c)