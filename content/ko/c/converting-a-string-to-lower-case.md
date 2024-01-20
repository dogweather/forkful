---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
문자열을 소문자로 변경하는 것은 모든 철자를 소문자로 변환하는 것입니다. 프로그래머들은 텍스트를 비교하거나 정렬할 때 대소문자 구분 없이 작업하기 위해 이를 사용합니다.

## 어떻게 사용하나요:
```C
#include <ctype.h>
#include <stdio.h>
  
void lower_string(char s[]) {
  int c = 0;
   
  while (s[c] != '\0') {
    if (s[c] >= 'A' && s[c] <= 'Z') {
       s[c] = s[c] + 32;
    }
    c++;
  }
}

int main() {
  char string[] = "HELLO WORLD!";
  
  lower_string(string);
  printf("%s\n", string);

  return 0;
}
```
위 코드를 실행하면 출력 결과는 `hello world!`입니다.

## 심화정보
문자열의 소문자 변환 기능은 오래전부터 존재했으며, 텍스트 처리에 광범위하게 사용되고 있습니다. 이 과정은 컴퓨터 프로그래밍에서 흔히 발생하는 대소문자 문제를 해결하기 위해 사용되곤 합니다. 그 외에도 다른 대안들이 있다. 예를 들어, `tolower()` 함수를 각 문자에 적용하거나, 문자열 조작 라이브러리를 사용하는 등의 방법이 있습니다. 위의 코드 예제에서는 ASCII 값을 이용하여 대문자를 소문자로 변경하는 방법을 보였습니다. `A-Z`까지의 대문자 ASCII 값에 32를 더하면 소문자 ASCII 값이 됩니다.

## 참고 자료
* [C 표준 라이브러리 `ctype.h`에 대한 정보](https://en.cppreference.com/w/c/string/byte)