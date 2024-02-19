---
aliases:
- /ko/c/converting-a-string-to-lower-case/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:49.866167-07:00
description: "C\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\
  \uD658\uD558\uB294 \uAC83\uC740 \uC8FC\uC5B4\uC9C4 \uBB38\uC790\uC5F4\uC758 \uBAA8\
  \uB4E0 \uB300\uBB38\uC790\uB97C \uD574\uB2F9\uD558\uB294 \uC18C\uBB38\uC790\uB85C\
  \ \uBCC0\uD658\uD558\uB294 \uACFC\uC815\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\
  \uD558\uC5EC \uD14D\uC2A4\uD2B8 \uC785\uB825\uC744 \uD45C\uC900\uD654\uD558\uAC70\
  \uB098 \uBE44\uAD50, \uAC80\uC0C9 \uC791\uC5C5\uC744 \uC704\uD574, \uB610\uB294\
  \ \uB2E8\uC21C\uD788 \uCD9C\uB825\uC5D0\uC11C\uC758 \uC678\uAD00\uC801 \uC77C\uAD00\
  \uC131\uC744 \uC704\uD574 \uC218\uD589\uD569\uB2C8\uB2E4."
lastmod: 2024-02-18 23:09:06.928381
model: gpt-4-0125-preview
summary: "C\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\
  \uD558\uB294 \uAC83\uC740 \uC8FC\uC5B4\uC9C4 \uBB38\uC790\uC5F4\uC758 \uBAA8\uB4E0\
  \ \uB300\uBB38\uC790\uB97C \uD574\uB2F9\uD558\uB294 \uC18C\uBB38\uC790\uB85C \uBCC0\
  \uD658\uD558\uB294 \uACFC\uC815\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uC885\uC885 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD558\
  \uC5EC \uD14D\uC2A4\uD2B8 \uC785\uB825\uC744 \uD45C\uC900\uD654\uD558\uAC70\uB098\
  \ \uBE44\uAD50, \uAC80\uC0C9 \uC791\uC5C5\uC744 \uC704\uD574, \uB610\uB294 \uB2E8\
  \uC21C\uD788 \uCD9C\uB825\uC5D0\uC11C\uC758 \uC678\uAD00\uC801 \uC77C\uAD00\uC131\
  \uC744 \uC704\uD574 \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC744 \uC18C\uBB38\uC790\uB85C \uBCC0\uD658\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?

C에서 문자열을 소문자로 변환하는 것은 주어진 문자열의 모든 대문자를 해당하는 소문자로 변환하는 과정을 말합니다. 프로그래머들은 종종 이 작업을 수행하여 텍스트 입력을 표준화하거나 비교, 검색 작업을 위해, 또는 단순히 출력에서의 외관적 일관성을 위해 수행합니다.

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
