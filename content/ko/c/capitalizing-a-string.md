---
title:                "문자열 대문자로 변환하기"
html_title:           "C: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜
요즘 대부분의 프로그래밍 언어에서는 문자열을 제어하는 데 많은 기능을 제공합니다. 이 중 하나는 문자열의 첫 글자를 대문자로 변환하는 것입니다. 이 기능을 사용하면 문자열을 더 다양한 방식으로 표현할 수 있고 유용한 기능을 구현할 수 있습니다.

## 하는 방법
```C
#include <stdio.h>
#include <string.h>

int main()
{
    char str[20] = "hello, world!";

    // 첫 글자를 대문자로 변환하는 함수
    str[0] = toupper(str[0]);

    printf("변환된 문자열: %s", str);

    return 0;
}
```

위 코드를 실행하면 "Hello, world!"라는 출력 결과를 볼 수 있습니다.

## 깊게 파고들기
대문자로 변환하는 기능은 문자열을 처리하는 프로그램에서 매우 유용합니다. 예를 들어, 사용자가 입력한 이름을 받은 후, 이를 대문자로 변환하여 데이터베이스와 비교하거나 직접 파일 이름을 적을 때 유용합니다. 또한, 소문자와 대문자가 혼용된 큰 데이터셋을 다룰 때 특정 문자열만 뽑아내기 위해 사용할 수 있습니다.

## 더 알아보기
- [C 언어 공식 문서](https://www.iso.org/standard/74528.html)
- [문자열 함수에 대한 자세한 설명](https://www.geeksforgeeks.org/string-functions-in-c-with-examples/) 
- [문자열 변환 예제 코드](https://www.programiz.com/c-programming/examples/uppercase-string)