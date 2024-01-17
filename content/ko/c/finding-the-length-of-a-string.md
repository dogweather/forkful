---
title:                "문자열의 길이 찾기"
html_title:           "C: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# What & Why?
문자열의 길이를 찾는 것은 그 이름 그대로, 특정 문자열이 얼마나 긴지를 알아내는 것을 말합니다. 이 기능은 프로그래밍에서 매우 중요한데, 우리가 다루는 데이터가 얼마나 길거나 얼마만큼의 용량을 차지하는지 알아야 하기 때문입니다.

# How to:
예를 들어 문자열 "Hello World"의 길이를 알고 싶다면, 다음과 같이 코드를 작성할 수 있습니다.
```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hello World";
    int length = strlen(str);
    printf("Length of string: %d \n", length);
}
```
위 코드를 실행하면 다음과 같은 결과가 출력됩니다.
```
Length of string: 11
```

# Deep Dive:
이 기능은 C 언어에서 기본적으로 제공되는 함수인 `strlen()` 을 이용해 구현할 수 있습니다. 이 함수는 `<string.h>` 헤더 파일에 정의되어 있으며, 주어진 문자열의 길이를 반환합니다. 반대로, 문자열의 실제 크기를 구한다면 `sizeof()` 함수를 사용할 수 있지만 이 함수는 NULL 문자를 포함한 전체 배열의 크기를 반환하기 때문에 문자열의 길이를 찾는 데는 적합하지 않습니다. 또 다른 대안은 반복문을 사용해 문자열의 각 문자가 나타날 때마다 카운터를 증가시켜 길이를 찾는 것입니다.

# See Also:
- [strlen() 함수 문서](https://www.tutorialspoint.com/c_standard_library/c_function_strlen.htm)
- [sizeof() 함수 문서](https://www.tutorialspoint.com/c_standard_library/c_function_sizeof.htm)