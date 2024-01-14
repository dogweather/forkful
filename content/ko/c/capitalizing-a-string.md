---
title:    "C: 문자열 대문자 변환하기"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

재미있는 프로그래밍 세상에 오신 것을 환영합니다! 오늘은 C 프로그래밍에서 문자열을 대문자로 변환하는 방법에 대해 알아보겠습니다. 이 기능을 알면 대문자 문자열을 다루는 데 더 편리하고 쉬워질 것입니다.

## 왜 대문자 문자열을 사용해야 할까요?

대문자 문자열은 다양한 프로그래밍 과제에서 유용합니다. 예를 들어 사용자의 입력값을 대문자로 변환하면 입력값을 더 쉽게 처리할 수 있습니다. 또는 대문자 문자열을 사용하면 특정 문자열을 비교하기 더 쉬워집니다. 이 기능은 C 프로그래밍에서 자주 사용됩니다.

## 대문자 문자열 변환하는 방법

대문자 문자열을 변환하는 방법은 간단합니다. 우선, 문자열을 입력 받은 다음에 각 문자를 대문자로 변환하면 됩니다. 아래 예제를 확인해보세요.

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main() {
  char str[100];
  printf("문자열을 입력하세요: ");
  fgets(str, 100, stdin); // 문자열 입력 받기
  for(int i = 0; i < strlen(str); i++) {
    str[i] = toupper(str[i]); // 각 문자를 대문자로 변환
  }
  printf("대문자 변환 결과: %s", str);
  return 0;
}
```

위의 예제에서는 `toupper()` 함수를 사용하여 문자를 대문자로 변환했습니다. `toupper()` 함수는 `<ctype.h>` 라이브러리에 포함되어 있습니다. 입력값으로 받은 문자열을 `for` 루프를 통해 한 문자씩 읽어서 `toupper()` 함수를 적용하는 것입니다. 그리고 변환된 문자열을 출력했습니다. 위의 예제 코드를 실행하면 다음과 같은 결과가 나타납니다.

```
문자열을 입력하세요: Hello World
대문자 변환 결과: HELLO WORLD
```

## 대문자 문자열 변환에 대해 더 알아보기

더 깊이 들어가서 대문자 문자열을 변환하는 방법에 대해 알아보겠습니다. C 프로그래밍에서는 문자열을 배열로 다룹니다. 따라서 문자열의 각 문자는 배열의 원소로 존재하게 됩니다. 문자열의 각 자리에는 ASCII 코드가 적용되는데, 대문자와 소문자의 ASCII 코드는 서로 다릅니다. 따라서 대문자로 변환하고 싶은 문자에 대해 32를 빼주면 대문자로 변환됩니다. `tolower()` 함수를 사용하여 반대로 소문자로 변환하는 것도 가능합니다.

또한, 대문자와 소문자의 ASCII 코드 간의 관계를 이용해 문자열을 비교하는 것도 가능합니다. 예를 들어, "apple"과 "APPLE"을 비교하려면 ASCII 코드 간의 차이를 무시하고 비교해야 합니다. 이를 위해 `strcasecmp()` 함수를 사용할 수 있습니다. 이 함수는 대소문자 구분 없이 문자열을 비교해줍니다.

## See Also (관련 링크)

- [ASCII Table](https://www.asciitable.com/)
- [C Strings Tutorial](https://www.learn-c.org/en/Strings)
- [C String Functions](https://www.tutorialspoint.com/c_standard_library/c_function_strtoupper.htm)