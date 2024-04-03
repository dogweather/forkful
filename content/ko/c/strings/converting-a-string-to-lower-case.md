---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:49.866167-07:00
description: "\uBC29\uBC95: C\uB294 \uBA87\uBA87 \uACE0\uAE09 \uC5B8\uC5B4\uC640\uB294\
  \ \uB2EC\uB9AC \uBB38\uC790\uC5F4\uC744 \uC9C1\uC811 \uC18C\uBB38\uC790\uB85C \uBCC0\
  \uD658\uD558\uB294 \uB0B4\uC7A5 \uD568\uC218\uB97C \uAC00\uC9C0\uACE0 \uC788\uC9C0\
  \ \uC54A\uC2B5\uB2C8\uB2E4. \uADF8\uB7EC\uB098, C \uD45C\uC900 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC774 \uACFC\uC815\uC744\
  \ \uC27D\uAC8C \uAD6C\uD604\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC544\uB798\uC5D0\
  \uB294 \uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uB294\
  \ \uBC29\uBC95\uACFC \uC608\uC81C\uB97C \uB2E8\uACC4\uBCC4\uB85C \uC548\uB0B4\uD558\
  \uACE0 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.900499-06:00'
model: gpt-4-0125-preview
summary: "C\uB294 \uBA87\uBA87 \uACE0\uAE09 \uC5B8\uC5B4\uC640\uB294 \uB2EC\uB9AC\
  \ \uBB38\uC790\uC5F4\uC744 \uC9C1\uC811 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\
  \uB294 \uB0B4\uC7A5 \uD568\uC218\uB97C \uAC00\uC9C0\uACE0 \uC788\uC9C0 \uC54A\uC2B5\
  \uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 4
---

## 방법:
C는 몇몇 고급 언어와는 달리 문자열을 직접 소문자로 변환하는 내장 함수를 가지고 있지 않습니다. 그러나, C 표준 라이브러리 함수를 사용하여 이 과정을 쉽게 구현할 수 있습니다. 아래에는 문자열을 소문자로 변환하는 방법과 예제를 단계별로 안내하고 있습니다.

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower(*str);
        str++;
    }
}

int main() {
    char text[] = "Hello, World!";
    printf("원본: %s\n", text);

    toLowerCase(text);
    printf("소문자: %s\n", text);

    return 0;
}
```

**샘플 출력:**

```
원본: Hello, World!
소문자: hello, world!
```

이 예제에서, `toLowerCase` 함수는 입력 문자열의 각 문자를 순회하며 `ctype.h`의 `tolower` 함수를 사용해 해당 문자의 소문자로 변환합니다. 수정은 원래 문자열을 직접 변경하며 수행됩니다.

## 심층 분석
위의 예제에서 사용된 `tolower` 함수는 C 표준 라이브러리의 일부이며, 특히 `ctype.h` 헤더 파일 내에 있습니다. 이 함수는 현재 로케일에 기반하여 작동하지만, 표준 "C" 로케일의 경우에는 ASCII 문자 집합을 다루며 'A'에서 'Z'를 'a'에서 'z'로 변환합니다.

역사적으로, C에서의 문자 인코딩 및 대소문자 변환 처리는 ASCII 문자 집합과 밀접하게 연관되어 있었으며, ASCII 집합 외부의 문자가 흔한 국제적이거나 지역화된 애플리케이션에서는 이러한 처리의 유용성이 제한되었습니다. 현대 프로그래밍 언어는 로케일 및 유니코드 문자를 고려하여 대소문자 변환을 수행하는 내장 문자열 메소드를 제공할 수 있지만, C는 기본적으로 이를 가지고 있지 않습니다.

ASCII 문자 외의 광범위한 텍스트 조작이 필요한 시나리오에서 프로그래머는 ICU(국제 컴포넌트용 유니코드)와 같이 더 나은 국제화 지원을 제공하는 라이브러리를 사용하는 것을 고려할 수 있습니다. 그러나 대부분의 ASCII 텍스트를 다루는 응용 프로그램의 경우, 보여준 접근 방식은 효율적이고 간단합니다. 이는 프로그래머에게 데이터 조작에 대한 제어를 제공하는 C의 특성을 강조하지만, 고급 언어에 비해 다소 더 많은 노력이 필요하다는 것을 보여줍니다.
