---
title:                "문자열 보간하기"
html_title:           "Java: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

문자열 보간(string interpolation)은 문자열 내에서 변수나 표현식을 사용하여 새로운 문자열을 생성하는 프로그래밍 기술입니다. 프로그래머들은 이를 사용하여 코드를 간결하게 하고 더욱 가독성 좋게 만들기 위해 사용합니다.

## 사용 방법:

C에서 우리는 `printf` 함수를 사용하여 문자열을 보간할 수 있습니다. 예제를 살펴봅시다:

```C 
#include <stdio.h>

int main() {
    int age = 25;
    printf("내 나이는 %d이다.\n", age);
    return 0;
}
```

출력:

```
내 나이는 25이다.
```

위의 코드에서 `%d`는 `printf` 함수가 `age` 변수의 값을 가져와 문자열에 삽입할 위치를 나타냅니다.

## 깊게 알아보기

문자열 보간은 많은 프로그래밍 언어에 있으며 각 언어는 이 기능을 제공하는 고유한 방식을 가지고 있습니다. C언어의 `printf` 함수는 문자열 보간의 대표적인 예입니다.

대안으로, 문자열 연결 또는 포매팅 함수를 사용할 수도 있습니다. 하지만 이런 방법들은 복잡하고 버그를 유발하기 쉬울 수 있습니다.

C에선 실제로 문자열 보간이 단순 문자열 치환일 뿐 아니라 더 깊은 수준에서 처리됩니다. 컴파일러는 `%` 기호 뒤에 오는 형식 지정자를 찾아 적절한 값으로 변환하고 이를 문자열에 보간합니다.