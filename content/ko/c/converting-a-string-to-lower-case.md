---
title:                "문자열을 소문자로 변환하기"
html_title:           "C: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜?

문자열을 소문자로 변환하는 이유는 다양할 수 있습니다. 영문 대소문자 구분을 하지 않는 경우 데이터를 일관되게 처리하기 위해서, 또는 사용자가 입력한 문자열을 비교하기 전에 일관성을 유지하기 위해서 등등 다양한 이유가 있습니다.

## 어떻게?

가장 간단한 방법은 `toupper()` 함수를 사용하는 것입니다. 이 함수는 ASCII 문자열을 대문자로 변환해주는 역할을 합니다. 하지만 이 함수는 이미 대문자인 문자는 건들지 않기 때문에 소문자로 변환하려면 다른 방법을 사용해야 합니다.

다음은 `tolower()` 함수를 사용하여 소문자로 변환하는 예제입니다.

```c
#include <stdio.h>
#include <string.h>

int main() {
    char my_str[] = "Hello, World";
    int len = strlen(my_str);

    for (int i = 0; i < len; i++) {
        my_str[i] = tolower(my_str[i]);
    }

    printf("%s", my_str); // 출력: hello, world

    return 0;
}
```

위 예제에서 `strlen()` 함수를 사용하여 문자열의 길이를 구하고, 반복문을 사용하여 모든 문자를 `tolower()` 함수를 통해 소문자로 변환합니다. 그 후 변환된 문자열을 `printf()` 함수를 사용하여 출력합니다.

## 깊게 파고들기

C 언어에서 문자열을 소문자로 변환하는 방법은 여러 가지가 있습니다. 하지만 위에서 작성한 예제처럼 `toupper()` 또는 `tolower()` 함수를 사용하는 것이 가장 간단한 방법입니다.

또 다른 방법으로는 `std::transform()` 함수를 사용하는 방법이 있습니다. 이 함수는 표준 C++ 라이브러리에 포함되어 있지만, C 언어에서도 `#include <algorithm>`을 추가하여 사용할 수 있습니다. 이 함수는 두 개의 반복자를 인자로 받아서 해당 범위의 값을 변환하는 역할을 합니다. 다음은 위 예제를 `std::transform()`을 사용하여 작성한 경우입니다.

```c
#include <stdio.h>
#include <string.h>
#include <algorithm>

int main() {
    char my_str[] = "Hello, World";
    int len = strlen(my_str);

    std::transform(my_str, my_str + len, my_str, ::tolower);

    printf("%s", my_str); // 출력: hello, world

    return 0;
}
```

위 예제에서는 `std::transform()` 함수를 사용하여 모든 문자를 소문자로 변환하였습니다.

## 참고자료

- [C 언어 | `toupper()` 함수](https://www.tutorialspoint.com/c_standard_library/c_function_toupper.htm)
- [C 언어 | `tolower()` 함수](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [Standard C++ Library | `std::transform()` 함수](https://www.cplusplus.com/reference/algorithm/transform/)