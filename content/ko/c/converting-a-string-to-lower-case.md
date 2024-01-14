---
title:    "C: 문자열을 소문자로 변환하기"
keywords: ["C"]
---

{{< edit_this_page >}}

## 왜

스트링을 소문자로 변환하려는 이유는 파이썬에서 데이터 처리 또는 데이터 분석을 할 때 유용할 수 있습니다.

## 방법

우선, `ctype.h` 헤더 파일을 포함시켜 아스키 코드 변환을 가능하게 만듭니다. 그 후에, `tolower()` 함수를 사용하여 문자열에 있는 모든 대문자를 소문자로 변환합니다. 아래는 C 언어로 이를 구현한 코드와 예시 출력입니다.

```C
#include <stdio.h>
#include <ctype.h>
#include <string.h>

int main()
{
    char str[100];
    printf("문자열을 입력하세요: ");
    scanf("%s", str);

    for (int i = 0; str[i] != '\0'; i++)
    {
        str[i] = tolower(str[i]);
    }

    printf("변환된 문자열: %s", str);

    return 0;
}
```

입력: HelloWorld
출력: helloworld

## 깊은 고찰

문자열을 소문자로 변환하는 것은 프로그래밍에서 자주 사용되는 작업 중 하나입니다. 이는 문자열의 대소문자를 비교할 때 유용하며, 대문자로 입력된 데이터를 소문자로 재정렬하는 등의 작업에 사용될 수 있습니다. 하지만 문자열이나 문자를 비교할 때에는 주의해야 합니다. 다른 언어로 작성한 문자열의 경우 정확한 변환이 이루어지지 않을 수 있으며, 이는 프로그램의 오류를 발생시킬 수 있습니다.

## 참고자료

- [C 함수 레퍼런스 - tolower](https://www.cplusplus.com/reference/cctype/tolower/)
- [Stack Overflow - How can I convert a string to lowercase in C?](https://stackoverflow.com/questions/351733/how-can-i-convert-a-string-to-lowercase-in-c)