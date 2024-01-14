---
title:    "C: 부분 문자열 추출하기"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/extracting-substrings.md"
---

{{< edit_this_page >}}

# 왜 추출할까요?

C 언어를 배우는 과정에서 많은 경우에 문자열에서 일부분을 추출하는 것이 필요하게 됩니다. 이것을 효율적으로 처리할 수 있으면 더 많은 데이터를 다루는 프로그래밍에 유용합니다.

## 추출하는 법

문자열에서 일부분을 추출하는 방법은 다양합니다. 가장 간단한 방법은 하나의 문자를 추출하는 것입니다. 다음과 같은 코드를 사용할 수 있습니다.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hello, World!";
    char ch;

    // 문자열에서 'W' 추출
    ch = str[7];

    printf("추출한 문자는 %c입니다.", ch);

    return 0;
}
```

출력 결과는 다음과 같습니다.

```
추출한 문자는 W입니다.
```

이외에도 다양한 방법으로 문자열에서 일부분을 추출할 수 있습니다. `strcpy()` 함수를 사용하여 substring을 새로운 문자열로 복사하는 방법이 있습니다. 또는 `strncpy()` 함수를 사용하여 원하는 길이만큼 문자열을 복사할 수도 있습니다.

## 깊게 파고들기

C 언어에서 문자열에서 일부분을 추출하는 과정은 유용하지만 관련해서 몇 가지 주의할 점이 있습니다. 예를 들어, 추출하려는 substring의 인덱스가 문자열의 길이보다 크면 오류가 발생할 수 있습니다. 따라서 항상 문자열의 길이를 먼저 확인하는 것이 좋습니다. 또한 제대로 추출된 substring이 원하는 값과 일치하는지도 확인하는 것이 중요합니다.

또 다른 중요한 주의점은 메모리 관리입니다. 문자열에서 일부분을 추출한 후 새로운 문자열로 복사하면서 메모리가 낭비될 수 있습니다. 따라서 메모리 관리에 유의하여 코드를 작성하는 것이 중요합니다.

# 이 외에도 알아보기

- [프로그래밍 언어 C](https://ko.wikipedia.org/wiki/C)
- [문자열 관련 함수](https://www.tutorialspoint.com/c_standard_library/string_h.htm)