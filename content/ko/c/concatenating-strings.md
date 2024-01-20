---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/concatenating-strings.md"
---

{{< edit_this_page >}}

# C 언어 문자열 연결하기

## 무엇이며 왜 해야하는가?

문자열을 연결하는 것은 두 개 이상의 문자열을 한 줄로 붙이는 과정입니다. 이는 정보를 세련되게 출력하거나 데이터를 특정 형식으로 조립하는 등의 상황에서 유용하게 사용됩니다.

## 어떻게 하는가?

C 언어에서는 `strcat()` 이라는 내장 함수를 이용해 문자열을 쉽게 연결할 수 있습니다. 다음은 이 함수를 사용하는 예제입니다:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char s1[100] = "안녕하세요, ";
    char s2[] = "세계!";
    strcat(s1, s2);
    printf("%s\n", s1);
    return 0;
}
```

이 코드를 실행하면 다음과 같은 출력을 얻습니다:

```
안녕하세요, 세계!
```

## 깊게 알아보기

`strcat()` 함수는 C의 표준 라이브러리인 <string.h>에 포함되어 있습니다. 처음에는 1972년, C언어가 처음 등장했을 때부터 사용되어 오고 있습니다.

`strcat()` 대신에 `strncat()` 함수를 사용하면 몇 개의 문자를 연결할지 제한할 수 있다는 장점이 있습니다. 이는 보안 문제 등의 이유로 중요합니다.

또한, `strcat()` 함수는 보통 아래와 같이 동작합니다:
1. 첫 번째 문자열의 끝을 찾습니다.
2. 두 번째 문자열의 첫 번째 문자부터 복사를 시작합니다.
3. 두 번째 문자열의 끝에 도달하면 복사를 중단하고 첫 번째 문자열의 원래 끝에 NULL 문자를 추가합니다.

## 참고 자료

- [C 언어에서의 문자열 핸들링](https://www.geeksforgeeks.org/string-handling-in-c/)
- [C 언어의 strcat와 strncat 함수](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [string.h 라이브러리 도큐먼트](https://www.gnu.org/software/libc/manual/html_node/String-Functions.html)