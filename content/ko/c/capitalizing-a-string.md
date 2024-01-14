---
title:                "C: 문자열 대문자화하기"
simple_title:         "문자열 대문자화하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜
문자열을 대문자로 변환하는 것에 대해 이야기해보겠습니다. 

## How To
문자열을 대문자로 변환하는 방법은 매우 간단합니다. `strcpry()` 함수를 사용하면 됩니다. 다음은 간단한 예제 코드와 출력 결과입니다.

```
#include <stdio.h>
#include <string.h>

int main(void) {
  char str[] = "hello world";
  printf("%s\n", str);

  // 문자열을 대문자로 변환
  strupr(str);
  printf("%s\n", str);

  return 0;
}

/* 출력 결과
hello world
HELLO WORLD
*/
```

위의 코드에서 `strupr()` 함수를 사용하여 문자열을 대문자로 변환할 수 있습니다. 이 함수는 `string.h` 라이브러리에 포함되어 있으며, 문자열을 대문자로 변환하는 기능을 합니다.

## Deep Dive
문자열을 대문자로 변환하기 위해 `strupr()` 함수가 어떻게 작동하는지 깊이 알아보겠습니다. 이 함수는 주어진 문자열을 순회하면서, 각 문자를 `toupper()` 함수를 사용하여 대문자로 변환합니다. 그 후, 변환된 문자열을 반환합니다.

또한, 이 함수를 사용하여 변환되는 문자가 알파벳이 아닌 경우에는 변환하지 않습니다. 즉, 스페이스, 숫자, 특수 문자 등은 변환되지 않는 것을 확인할 수 있습니다.

결과적으로, `strupr()` 함수는 주어진 문자열을 대문자로 변환하는 역할을 하며, 각 문자를 일일이 변환하는 과정을 거치게 됩니다.

## See Also
- [C string 함수 - strupr()](https://www.tutorialspoint.com/c_standard_library/c_function_strupr.htm)
- [C 언어 - 문자열(str) 함수](https://dojang.io/mod/page/view.php?id=311)