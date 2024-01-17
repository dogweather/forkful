---
title:                "문자열의 첫 글자를 대문자로 만들기"
html_title:           "C: 문자열의 첫 글자를 대문자로 만들기"
simple_title:         "문자열의 첫 글자를 대문자로 만들기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열에 대문자화는 무엇인가요? 그리고 프로그래머들이 왜 이것을 하는 걸까요? 

대문자화란, 문자열의 각 문자를 대문자로 변환하는 것을 말합니다. 보통 이 작업은 대소문자 구분이 필요한 경우, 특히 비밀번호나 사용자 이름 등의 인증 정보를 입력받을 때 사용됩니다. 프로그래머들은 이 작업을 통해 사용자의 입력을 안전하게 처리할 수 있도록 합니다.

## 사용 방법:

바로 들어가서 코드로 예를 들어보겠습니다. 

```C
#include <stdio.h>

int main()
{
    char word[10] = "hello";
    int i;

    for (i = 0; i < 5; i++)
    {
        word[i] = word[i] - 32; // 대문자로 변환
    }

    printf("%s", word); // HELLO 출력
    
    return 0;
}
```

위의 코드를 실행하면 "HELLO"라는 결과를 볼 수 있습니다. 이와 같이 문자열 내의 모든 문자를 대문자로 변환하는 것이 대문자화의 기본적인 사용법입니다.

## 더 깊게 들어가보기:

대문자화는 흔히 사용되는 문자열의 가장 기본적인 처리 중 하나입니다. 그래서 대부분의 언어에서는 내장 함수로 제공하고 있습니다. 예를 들어, JavaScript에서는 `toUpperCase()`라는 함수를 통해 문자열을 대문자로 변환할 수 있습니다. 또 다른 대안으로는 ASCII 테이블을 이용하여 문자의 아스키 코드 값을 변경해주는 방법이 있습니다.

C에서 대문자화는 ASCII 코드 값을 변경하여 대문자로 변환하는 방식으로 이루어집니다. `char` 변수의 범위는 -128에서 127까지이므로, 이 범위를 넘어가는 경우 형 변환을 거쳐 값을 변경해야 합니다. 또한, 유니코드를 지원하는 최신 언어들에서는 대문자화가 좀 더 복잡해지게 됩니다. 이 경우에는 해당 언어에서 제공하는 내장 함수를 사용하거나, 매핑 테이블을 이용하여 대문자로 변환할 수 있습니다.

## 관련 자료:

- [C string functions](https://www.tutorialspoint.com/c_standard_library/c_function_strcmp.htm)
- [ASCII table](https://www.w3schools.com/charsets/ref_html_ascii.asp)
- [ASCII 코드에 대한 보다 깊은 이해](https://konradsob.wordpress.com/2013/05/09/ascii-codes-useful-shortcuts-for-developers/)