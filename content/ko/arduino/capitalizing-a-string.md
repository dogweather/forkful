---
title:                "문자열 대문자 변환하기"
html_title:           "Arduino: 문자열 대문자 변환하기"
simple_title:         "문자열 대문자 변환하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜

문자열을 대문자로 바꾼다는 것은 문자열의 모든 문자를 대문자로 변환하는 것을 의미합니다. 프로그래머들은 이를 자주 사용하는 이유는 두 가지가 있습니다. 첫째, 대문자로 된 문자열을 사용하면 더욱 더 쉽게 읽을 수 있습니다. 둘째, 대문자로 된 문자열을 사용하는 것은 일관된 형식을 유지하고 코드를 더욱 깔끔하게 만들 수 있습니다.

## 사용 방법

```Arduino
String myString = "hello world";
myString.toUpperCase(); // 출력: HELLO WORLD
```

## 깊이 파고들기

대문자로 문자열을 바꾸는 것은 오래된 컴퓨터 시스템에서 사용되던 기능입니다. 이 기능은 전통적으로 문자열의 첫 번째 문자를 대문자로 만들고 나머지 문자는 모두 소문자로 변환하는 방식으로 작동합니다. 대문자로 문자열을 바꾸는 대신, 프로그래머는 문자열 내의 각 문자를 개별적으로 변환하는 대안을 사용할 수도 있습니다. 이는 일부 언어에서 더욱 효율적일 수 있습니다.

## 관련 자료

- [우아한형제들 기술 블로그 - 문자열 대/소문자 변환](https://woowabros.github.io/tools/2017/07/17/to_upper_lower.html)
- [C Language Tutorial - String Functions](https://www.tutorialspoint.com/cprogramming/c_string_functions.htm)