---
title:                "인터랙티브 셸 (REPL) 사용하기"
date:                  2024-01-26T04:12:07.131663-07:00
model:                 gpt-4-0125-preview
simple_title:         "인터랙티브 셸 (REPL) 사용하기"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
인터랙티브 셸 또는 Read-Eval-Print Loop (REPL)은 코드 조각을 즉시 테스트할 수 있는 실시간 코딩 환경을 제공하는 도구입니다. 프로그래머는 개발, 학습 및 디버깅 중에 빠른 피드백을 위해 이를 사용합니다.

## 방법:
C는 내장 REPL을 제공하지 않지만, 타사 도구를 사용할 수 있습니다. C++ 인터프리터이며 C 코드도 처리할 수 있는 Cling을 사용한 예시입니다:

```C
#include <stdio.h>

int main() {
    printf("Hello, REPL world!\n");
    return 0;
}
```

Cling REPL에서의 출력:
```
[cling]$ .x yourscript.c
Hello, REPL world!
```

Cling은 스크립트를 실행하고 출력을 즉시 출력합니다.

## 심층 탐구
Python이나 Ruby와 같은 동적 언어에서는 REPL이 표준이지만, C와 같은 컴파일 언어에서는 덜 흔합니다. 역사적으로, 컴파일-실행-디버그 사이클은 인터랙티브 탐구에 적합하지 않았습니다. Cling과 온라인 C 컴파일러와 같은 도구는 C++ 환경으로 C 코드를 래핑함으로써 REPL 같은 경험을 제공합니다.

Cling에 대한 대안으로는 CINT와 Ch와 같은 C 인터프리터가 있습니다. 이 도구들은 빠른 반복을 허용하지만 성능 제약과 복잡한 기능에 대한 지원 때문에 모든 개발 시나리오에 적합하지 않을 수 있습니다.

컴파일 언어에서 REPL의 구현은 즉석에서 코드 조각을 컴파일하고 실행하는 것을 포함하며, 이는 사소하지 않으며 전체 언어 기능에 비해 제한이 있을 수 있습니다.

## 참고하기
- Cling: https://github.com/root-project/cling
- 온라인 C 컴파일러 및 REPL: https://repl.it/languages/c
- CINT: http://root.cern.ch/drupal/content/cint
- Ch 인터프리터: http://www.softintegration.com/products/chstandard/
