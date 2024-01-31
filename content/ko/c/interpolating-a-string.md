---
title:                "문자열 보간하기"
date:                  2024-01-20T17:50:44.475323-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 보간하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 그리고 왜 사용하나요?)
문자열 보간(string interpolation)은 문자열 안에 변수나 표현식의 결과를 삽입하는 과정입니다. 이런 방법으로 코드를 보다 읽기 쉽고 유지보수하기 편해집니다.

## How to: (어떻게 사용하나요?)
C 언어에는 내장된 문자열 보간 기능이 없지만, `printf` 함수나 `sprintf` 함수를 사용하여 유사한 작업을 수행할 수 있습니다.

```C
#include <stdio.h>

int main() {
    char name[] = "홍길동";
    int age = 20;

    // printf를 사용한 문자열 보간
    printf("이름: %s, 나이: %d\n", name, age);

    // sprintf를 사용하여 문자열에 결과를 저장
    char info[50];
    sprintf(info, "이름: %s, 나이: %d", name, age);

    printf("%s\n", info);

    return 0;
}
```

출력:
```
이름: 홍길동, 나이: 20
이름: 홍길동, 나이: 20
```

## Deep Dive (심층 분석)
C언어에서 문자열 보간이 직접 지원되지 않는 이유는 C의 저수준 언어 특성과 간결함 때문입니다. 대신에 `printf`와 `sprintf` 같은 함수를 이용할 수 있습니다. 이러한 함수들은 `%s`, `%d`와 같은 형식 지정자를 사용하여 문자열에 변수 값을 삽입합니다.

예전에는 문자열 처리에 다소 불편함이 있었다는 점이 흥미롭습니다. 그러나 현대 C는 표준 라이브러리의 발전으로 이러한 작업을 용이하게 만들었습니다. 그 외에도 몇몇 타 언어들은 문자열 보간을 언어 자체의 기능으로 제공하기도 합니다.

구현 측면에서, C의 문자열은 실제로는 문자 배열이며, `printf`와 `sprintf`는 메모리에서 위치를 찾아 값을 치환하는 방식을 사용하므로 문자열 보간 작업은 메모리 안전성을 확인하면서 신중하게 수행되어야 합니다.

## See Also (관련 링크)
- C Standard Library functions: [https://en.cppreference.com/w/c/io/fprintf](https://en.cppreference.com/w/c/io/fprintf)
- C String Formatting: [https://www.cplusplus.com/reference/cstdio/printf/](https://www.cplusplus.com/reference/cstdio/printf/)
