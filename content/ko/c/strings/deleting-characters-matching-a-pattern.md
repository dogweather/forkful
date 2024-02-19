---
aliases:
- /ko/c/deleting-characters-matching-a-pattern/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:00.902634-07:00
description: "C\uC5D0\uC11C \uD2B9\uC815 \uD328\uD134\uC5D0 \uB9DE\uB294 \uBB38\uC790\
  \ \uC0AD\uC81C\uD558\uAE30\uB294 \uC0AC\uC804\uC5D0 \uC815\uC758\uB41C \uAE30\uC900\
  \uC5D0 \uBD80\uD569\uD558\uB294 \uD2B9\uC815 \uBB38\uC790\uB4E4\uC758 \uBAA8\uB4E0\
  \ \uC778\uC2A4\uD134\uC2A4\uB97C \uBB38\uC790\uC5F4\uC5D0\uC11C \uC81C\uAC70\uD558\
  \uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB294 \uC785\uB825\uAC12\uC744 \uC815\uD654\uD558\uAC70\uB098, \uCC98\uB9AC\
  \uB97C \uC704\uD55C \uB370\uC774\uD130\uB97C \uC900\uBE44\uD558\uAC70\uB098, \uCD9C\
  \uB825\uC774\uB098 \uCD94\uAC00\uC801\uC778 \uC870\uC791\uC744 \uC704\uD574 \uBB38\
  \uC790\uC5F4\uC744 \uC815\uB9AC\uD558\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744\
  \u2026"
lastmod: 2024-02-18 23:09:06.922890
model: gpt-4-0125-preview
summary: "C\uC5D0\uC11C \uD2B9\uC815 \uD328\uD134\uC5D0 \uB9DE\uB294 \uBB38\uC790\
  \ \uC0AD\uC81C\uD558\uAE30\uB294 \uC0AC\uC804\uC5D0 \uC815\uC758\uB41C \uAE30\uC900\
  \uC5D0 \uBD80\uD569\uD558\uB294 \uD2B9\uC815 \uBB38\uC790\uB4E4\uC758 \uBAA8\uB4E0\
  \ \uC778\uC2A4\uD134\uC2A4\uB97C \uBB38\uC790\uC5F4\uC5D0\uC11C \uC81C\uAC70\uD558\
  \uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB294 \uC785\uB825\uAC12\uC744 \uC815\uD654\uD558\uAC70\uB098, \uCC98\uB9AC\
  \uB97C \uC704\uD55C \uB370\uC774\uD130\uB97C \uC900\uBE44\uD558\uAC70\uB098, \uCD9C\
  \uB825\uC774\uB098 \uCD94\uAC00\uC801\uC778 \uC870\uC791\uC744 \uC704\uD574 \uBB38\
  \uC790\uC5F4\uC744 \uC815\uB9AC\uD558\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744\
  \u2026"
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C\uD558\
  \uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

C에서 특정 패턴에 맞는 문자 삭제하기는 사전에 정의된 기준에 부합하는 특정 문자들의 모든 인스턴스를 문자열에서 제거하는 것에 관한 것입니다. 프로그래머는 입력값을 정화하거나, 처리를 위한 데이터를 준비하거나, 출력이나 추가적인 조작을 위해 문자열을 정리하기 위해 이 작업을 수행합니다. 이를 통해 주어진 컨텍스트나 알고리즘에 필요한 정확한 데이터를 처리할 수 있습니다.

## 방법:

C는 패턴을 기반으로 문자열에서 문자를 직접 삭제하기 위한 내장 함수를 제공하지 않습니다. 이는 다른 고급 언어들과는 다릅니다. 그러나, 문자열을 수동으로 반복하며 원하지 않는 문자를 제외한 새로운 문자열을 구성함으로써 쉽게 이 작업을 수행할 수 있습니다. 예를 들어, 문자열에서 모든 숫자를 제거하고 싶다고 가정해 봅시다. 다음과 같이 할 수 있습니다:

```c
#include <stdio.h>
#include <ctype.h>

void remove_digits(char *str) {
    char *src = str, *dst = str;
    while (*src) {
        if (!isdigit((unsigned char)*src)) {
            *dst++ = *src;
        }
        src++;
    }
    *dst = '\0';
}

int main() {
    char str[] = "C 프로그래밍 101: 기초!";
    remove_digits(str);
    printf("결과: %s\n", str);
    return 0;
}
```

샘플 출력:
```
결과: C 프로그래밍 : 기초!
```

이 예제는 `isdigit`을 `ctype.h`에서 사용하여 숫자를 식별하고, 숫자가 아닌 문자를 문자열의 시작 부분으로 이동시키며, 모든 문자가 평가될 때까지 문자열을 종료합니다.

## 심층 분석

제시된 해결책은 원하지 않는 문자를 효과적으로 필터링하기 위해 동일한 배열 내에서 두 포인터 접근 방식을 사용합니다. 이는 C의 실용적인 메모리 관리 철학을 상징하는 기술입니다. 이 방법은 추가 메모리 할당이 필요 없고 오버헤드를 최소화하기 때문에 효율적입니다.

역사적으로, C에서 고급 문자열 조작 함수의 부재는 프로그래머가 메모리 수준에서 문자열 처리에 대한 깊은 이해를 개발하도록 강요했으며, 위와 같은 혁신적인 접근법을 이끌었습니다. 이는 더 큰 제어력과 효율성의 이점을 가지지만, 버퍼 오버플로우나 한 칸 차이 오류와 같은 더 큰 오류 위험을 수반합니다.

안전성과 보안을 강조하는 현대 개발 컨텍스트에서, 이러한 저수준 작업을 추상화하는 언어가 문자열 조작 작업에 선호될 수 있습니다. 그럼에도 불구하고, 세밀한 성능 최적화가 요구되거나 C의 최소주의와 속도가 최우선인 환경에서 작업할 때 이러한 C 기술을 이해하고 활용하는 것은 매우 가치 있는 일입니다.
