---
title:                "정규 표현식 사용하기"
html_title:           "Bash: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

정규 표현식은 텍스트 내에 있는 패턴을 찾고 조작하는 데 사용됩니다. 이는 데이터 검증, 검색 및 수정작업이 필요한 프로그래머에게 도움이 됩니다.

## 어떻게?

`regex.h`를 사용하여 C에서 정규 표현식을 사용할 수 있습니다. 하기 예제를 참조하십시오.

```C
#include <regex.h>  
#include <stdio.h>

void match_pattern(char* pattern, char* candidate) {
    regex_t regex_comp;
    regcomp(&regex_comp, pattern, REG_EXTENDED);
    
    if (REG_NOMATCH != regexec(&regex_comp, candidate, 0, NULL, 0)) {
        printf("\"%s\" matches pattern \"%s\"\n", candidate, pattern);
    } else {
        printf("\"%s\" does not match pattern \"%s\"\n", candidate, pattern);
    }

    regfree(&regex_comp);
}

int main() {
   char* pattern = "h[aeiou]llo";
   match_pattern(pattern, "hallo");
   match_pattern(pattern, "hello");
   match_pattern(pattern, "halo");

   return 0;
}
```
이 코드를 실행하면 다음과 같은 출력이 나옵니다.

```
"hallo" matches pattern "h[aeiou]llo"
"hello" matches pattern "h[aeiou]llo"
"halo" does not match pattern "h[aeiou]llo"
```

## 심화 학습

정규 표현식은 초기 시스템 통신의 필요성으로부터 비롯되었습니다. 다양한 언어, 플랫폼 및 도구에서 사용되어 왔습니다.

정규 표현식 대신 사용할 수 있는 것들은 문자열 검색 함수, 문자열 조작 함수, 운영 체제 함꼐 긴밀히 연결된 특정 일치 방식 등이 있습니다.

정규 표현식의 구현 세부사항은 사용되는 리소스, 속도, 정확성, 그리고 지원하는 기능을 포함한 것들에 따라 상당히 다릅니다.

## 참고 문헌

1. https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html
2. https://www.regular-expressions.info/tutorial.html
3. http://www.cs.princeton.edu/courses/archive/spr09/cos333/beautiful.html
4. https://www.pcre.org/original/doc/html/pcreapi.html