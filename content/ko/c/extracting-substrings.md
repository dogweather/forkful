---
title:                "부분 문자열 추출하기"
html_title:           "C: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/extracting-substrings.md"
---

{{< edit_this_page >}}

# 무엇이고 왜?

부분 문자열 추출이란 무엇인지 알아보고, 왜 프로그래머들이 이를 하는지 알아보겠습니다. 부분 문자열 추출은 문자열에서 특정 부분을 추출하는 것을 말합니다. 프로그래머들은 이 작업을 수행하여 문자열 데이터를 더 쉽게 처리하고, 필요한 정보만 추출할 수 있기 때문에 이를 자주 사용합니다.

# 어떻게:

다음은 C 프로그래밍에서 부분 문자열 추출을 하는 간단한 예제와 결과를 보여줍니다.

```C
#include <stdio.h> // printf 함수를 사용하기 위해 필요한 헤더 파일입니다.
#include <string.h> // strndup 함수를 사용하기 위해 필요한 헤더 파일입니다.

int main() {
    char str[] = "Hello World"; // 추출할 문자열입니다.
    int start = 0; // 추출을 시작할 인덱스입니다.
    int length = 5; // 추출할 길이입니다.
    char *substring = strndup(str + start, length); // 추출된 부분 문자열입니다.
    printf("Substring: %s\n", substring); // 결과를 출력합니다.
    return 0;
}
```

출력은 다음과 같을 것입니다.

```
Substring: Hello
```

# 깊이 있는 정보:

부분 문자열 추출은 문자열을 처리하는 데 있어 유용한 기능입니다. 예전에는 문자열에서 원하는 부분을 찾고 추출하는 작업이 까다로웠지만, 이제는 간단한 함수를 사용하여 쉽게 처리할 수 있게 되었습니다.

부분 문자열 추출을 위해 다른 방법도 있습니다. C 언어에서는 위와 비슷한 함수인 `strncpy`를 사용할 수 있습니다. 또는 `substr`라는 이름의 사용자 정의 함수를 만들어 사용할 수도 있습니다.

부분 문자열 추출을 구현하기 위해선, 문자열에서 인덱스를 이용해 원하는 부분을 잘라내는 방법을 사용합니다. 위의 예제에서는 `strndup` 함수를 사용하여 추출된 부분 문자열을 새로운 문자열로 만들어 주었습니다.

# 참고하십시오:

- [C substr 사용하기](https://www.tutorialspoint.com/c_standard_library/c_function_substr.htm)
- [C 에서 strndup 사용하기](https://www.tutorialspoint.com/c_standard_library/c_function_strndup.htm)