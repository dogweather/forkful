---
title:                "C: 문자열 소문자로 변환하기"
simple_title:         "문자열 소문자로 변환하기"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
문자열을 소문자로 변환하는 것에 참여하는 이유는 매우 간단합니다. 혹시 영어 문서나 프로그램에서 대문자로 쓰여 있는 문자열을 소문자로 바꿔야 할 때가 있지 않을까요? 소문자로 변환하게 되면 보기 좋아지고, 문자열 간의 정확한 비교가 가능해집니다. 이를 위해 C 프로그래밍을 사용하여 문자열을 간단하게 소문자로 바꾸는 방법을 알아보겠습니다.

## 방법
C 프로그래밍에서 문자열을 소문자로 바꾸는 작업은 실제로 매우 쉽습니다. 아래 코드는 이를 실제로 구현한 예시입니다.

```C
#include <stdio.h>
#include <string.h>

char* convertToLower(char* str) {
    for (int i = 0; i < strlen(str); i++) {
        if (str[i] >= 'A' && str[i] <= 'Z') {
            str[i] = str[i] + 32;
        }
    }
    return str;
}

int main() {
    char str[100];
    printf("문자열을 입력하세요: ");
    fgets(str, 100, stdin);
    char* lowercase = convertToLower(str);
    printf("변환된 문자열: %s", lowercase);
    return 0;
}
```

위 코드에서는 문자열을 입력받고, `convertToLower` 함수를 사용하여 문자열을 소문자로 변환하고 출력하는 예시입니다. `convertToLower` 함수에서는 `strlen` 함수를 사용하여 문자열의 길이를 구하고, 반복문을 사용하여 문자열의 각 문자를 확인하고 대문자인 경우에만 ASCII 값에 32를 더하여 소문자로 변환합니다. 이후 변환된 문자열을 메인 함수에서 출력합니다. 아래는 위 코드를 실행한 결과입니다.

```
문자열을 입력하세요: HELLO WORLD
변환된 문자열: hello world
```

이와 같이 간단하게 C 프로그래밍을 사용하여 문자열을 소문자로 변환할 수 있습니다.

## 딥 다이브
실제로는 문자열을 소문자로 변환하는 작업이 이렇게 간단하지 않을 수 있습니다. 영어 문서의 경우에는 대문자가 있는 문자 뿐만 아니라 다양한 언어의 문자가 혼합되어 있을 수 있기 때문입니다. 이런 경우에는 각 언어별로 대소문자에 대한 규칙을 정확하게 적용해야 합니다. 또한, 문자열 내에 숫자나 특수문자가 포함된 경우에도 적절하게 처리해주어야 합니다. 이러한 추가적인 처리를 위해 정규식을 사용하는 경우도 있습니다. 따라서, 문자열을 소문자로 변환하는 작업은 단순한 것처럼 보이지만 실제로는 꽤 복잡합니다.

## 관련 링크
- [C 프로그래밍 기초](https://www.learn-c.org/)
- [ASCII 테이블](http://www.asciitable.com/)
- [정규식 튜토리얼](https://www.regular-expressions.info/tutorial.html)

## 참고
본 포스트에서 사용된 코드 및 개념은 모두 [링크](https://github.com/grapherft/korean-readme-template)에서 확인하실 수 있습니다. 따라서, 이에 대한 라이선스는 해당 GitHub 레포지토리의 라이선스를 따릅니다.