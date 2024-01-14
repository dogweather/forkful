---
title:    "C: 문자열을 소문자로 변환하기"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜?

문자열을 소문자로 변환하는 것이 유용한 이유는 다양합니다. 예를 들어, 사용자로부터 입력받은 문자열을 일관된 형식으로 처리하고 비교할 때 유용합니다. 또한, 데이터베이스에서 대소문자를 구분하는 경우 입력값을 모두 소문자로 변환하여 검색 결과를 정확하게 얻을 수 있습니다.

## 어떻게?

C 언어에서 문자열을 소문자로 변환하는 방법에는 여러 가지가 있지만 가장 간단한 방법은 모든 문자를 ASCII 코드로 변환하고 해당 코드에서 32를 빼는 것입니다. 아래의 예시 코드를 확인해보세요.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[50];
    printf("문자열을 입력하세요: ");
    scanf("%s", str);

    for(int i=0; i < strlen(str); i++) {
        // 대문자 ASCII 코드인 65에서 97을 빼주어 소문자로 변환합니다.
        if(str[i] >= 'A' && str[i] <= 'Z') {
            str[i] = str[i] + 32;
        }
    }

    printf("변환된 문자열: %s", str);
    return 0;
}
```

### 출력 예시

```
문자열을 입력하세요: HELLOworld
변환된 문자열: helloworld
```

## 딥 다이브

문자열을 소문자로 변환하는 방법에 대해 더 깊게 알아보겠습니다. 위의 예시 코드에서는 ASCII 코드를 사용하여 문자를 변환하였지만, 유니코드에서는 더 다양한 문자를 지원하기 때문에 더 복잡한 과정이 필요합니다.

또한, 특수 문자나 공백 등의 예외 상황을 처리해주어야 합니다. 이를 위해 C 언어에서는 `tolower()` 함수를 제공하고 있습니다. 아래의 예시 코드를 확인해보세요.

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main() {
    char str[50];
    printf("문자열을 입력하세요: ");
    scanf("%s", str);

    for(int i=0; i < strlen(str); i++) {
        // 검사한 문자가 대문자인 경우에만 소문자로 변환합니다.
        str[i] = tolower(str[i]);
    }

    printf("변환된 문자열: %s", str);
    return 0;
}
```

### 출력 예시

```
문자열을 입력하세요: MyIdeaS
변환된 문자열: myideas
```

## 더 알아보기

여러분은 이제 문자열을 소문자로 변환하는 방법에 대해 잘 알게 되었습니다. 더 많은 정보를 얻기 위해 아래의 링크를 참고해보세요.

* [ASCII 코드 표](http://www.asciitable.com/)
* [C 언어의 string.h 라이브러리](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
* [C 언어의 ctype.h 라이브러리](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)

## 관련 자료

* [C 언어로 문자열을 역순으로 변환하는 방법](https://linktoexample.com)
* [C 언어에서 대소문자를 구분하지 않고 문자열을 비교하는 방법](https://linktoexample.com)
* [C 언어에서 문자열을 정렬하는 방법](https://linktoexample.com)