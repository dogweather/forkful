---
title:    "C: 문자열 결합하기"
keywords: ["C"]
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 일에 참여하는 *이유*는 무엇일까요? 간단히 말하자면, 문자열을 조작하는 작업을 수행할 때 서로 다른 문자열을 하나로 합치고 싶은 경우가 많기 때문입니다.

## 방법

C 프로그래밍에서 문자열을 연결하는 첫 번째 단계는 두 개의 문자열을 가지고 있어야 한다는 것입니다. 예를 들어, "안녕하세요"와 "반갑습니다"라는 두 개의 문자열이 있을 경우, 우리는 이 두 문자열을 하나의 문자열로 합칠 수 있습니다.

```C
#include <stdio.h>

int main() {
    char str1[30] = "안녕하세요";
    char str2[30] = "반갑습니다";

    strcat(str1, str2);

    printf("%s\n", str1);
    return 0;
}
```

출력:

```
안녕하세요반갑습니다
```

이번에는 조금 더 복잡한 예제를 살펴보겠습니다. 더 큰 문자열을 만들고 싶다면 어떻게 해야 할까요? 아래 예제에서는 3개의 문자열을 한 번에 연결하고 있습니다.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str1[100] = "오늘은";
    char str2[50] = "날씨가";
    char str3[20] = "매우 좋아요";

    strcat(strcat(str1, str2), str3);

    printf("%s\n", str1);
    return 0;
}
```

출력:

```
오늘은날씨가매우 좋아요
```

## 더 들어가보기

`strcat()` 함수의 정확한 작동 방식을 알고 싶다면 어떻게 될까요? 이 함수의 원형은 다음과 같습니다:

```
char *strcat(char *str1, char *str2)
```

여기서 `str1`과 `str2`는 합쳐질 문자열을 가리키는 포인터입니다. `strcat()` 함수는 먼저 `str1`의 끝을 찾은 다음, `str2`의 첫 번째 문자에서 `NULL` 문자를 복사하여 `str1`의 `NULL` 문자 다음에 붙입니다. 간단히 말해서, `str1`의 끝부터 `str2`의 시작까지 모든 문자를 차례대로 복사하여 하나의 문자열로 만드는 것입니다.

## See Also

- [String Concatenation in C](https://www.programiz.com/c-programming/library-function/string.h/strcat)
- [C String Functions](https://www.tutorialspoint.com/c_standard_library/c_function_strcat.htm)
- [Concatenating Strings in C](https://www.w3schools.in/c-tutorial/strings-2/)