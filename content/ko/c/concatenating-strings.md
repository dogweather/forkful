---
title:                "C: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/concatenating-strings.md"
---

{{< edit_this_page >}}

Korean Translation:

## 왜?

문자열 이어붙이기를 하는 이유는 무엇일까요? 단 1-2문장으로 이유를 설명해보겠습니다.

이는 매우 중요한 작업입니다. 우리는 언제나 프로그래밍에서 문자열을 다루게 되는데, 이어붙이기를 하는 것은 다양한 이유로 필요할 수 있습니다. 예를 들어, 사용자가 입력한 정보들을 하나의 문자열로 합치거나, 문자열을 특정한 형태로 정렬하는 등의 경우에 필요합니다. 이를테면 이메일 주소 구독 시에 여러 이메일 주소를 하나의 문자열로 이어붙여서 이를 손쉽게 처리하는 작업을 예시로 들 수 있습니다.

## 실제 코드 작성법

아래 코드 블록들에서는 C 프로그래밍 언어를 사용하여 문자열 이어붙이기를 하는 방법을 예시 코드와 함께 소개해 드리겠습니다.

```C
#include <stdio.h>
#include <string.h>

int main(void) {
    // 문자열 변수 선언 및 초기화
    char str1[50] = "Korean";
    char str2[50] = "Programming";
    
    // 문자열 이어붙이기
    strcat(str1, str2);
    
    // 결과 출력
    printf("New string: %s\n", str1);
    
    return 0;
}
```

위 코드는 "Korean"과 "Programming"이라는 두 문자열을 이어붙여 하나의 새로운 문자열을 만듭니다. 결과는 "KoreanProgramming"이 됩니다. 이 외에도 sprintf() 함수, 문자열 포맷 문자를 이용한 이어붙이기 등 다양한 방법으로 문자열을 이어붙일 수 있습니다.

## 깊게 파헤쳐보기

문자열 이어붙이기는 주로 두 가지 함수를 사용하게 됩니다. 바로 strcat() 함수와 sprintf() 함수입니다.

strcat() 함수는 기존 문자열에 새로운 문자열을 이어붙이는 함수로, 매우 간단한 방법이지만 공간에 대한 대비를 해야 합니다. 만약 기존 문자열에 충분한 공간이 없을 경우, 프로그램이 비정상적으로 종료될 수 있습니다.

sprintf() 함수는 기존 문자열에 새로운 문자열을 이어붙이는 것도 가능하지만, 더 많은 기능도 제공합니다. 이 함수는 문자열에 특정한 형태로 정보를 추가할 수 있습니다. 예를 들어, %d는 정수, %f는 실수, %c는 문자를 나타내는 문자열 포맷 문자입니다. sprintf() 함수는 대표적으로 파일 입출력에서 많이 사용되며, 대부분의 C 로직에서 필수적인 함수입니다.

## 다음으로 보기

[문자열 다루기 - strcat(), sprintf() 사용 방법](https://dojang.io/mod/page/view.php?id=250) <br/>
[C언어 문자열 - 문자열 이어붙이기](https://dojang.io/mod/page/view.php?id=249)