---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:27.718385-07:00
description: "\uC5B4\uB5BB\uAC8C: C REPL\uACFC \uC0C1\uD638\uC791\uC6A9\uD558\uB294\
  \ \uAC83\uC740 Python\uC774\uB098 JavaScript\uC640 \uAC19\uC740 \uC5B8\uC5B4\uB9CC\
  \uD07C \uC9C1\uAD00\uC801\uC774\uC9C0 \uC54A\uC744 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  . \uADF8\uB7EC\uB098 Clang\uACFC LLVM \uAE30\uC220\uC744 \uAE30\uBC18\uC73C\uB85C\
  \ \uD55C C/C++ \uC778\uD130\uD504\uB9AC\uD130\uC778 `Cling`\uACFC \uAC19\uC740 \uB3C4\
  \uAD6C\uB85C \uAC00\uB2A5\uD574\uC9D1\uB2C8\uB2E4. \uC2DC\uC791\uD558\uB294 \uBC29\
  \uBC95\uC740 \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4: 1.\u2026"
lastmod: '2024-03-13T22:44:55.926304-06:00'
model: gpt-4-0125-preview
summary: "C REPL\uACFC \uC0C1\uD638\uC791\uC6A9\uD558\uB294 \uAC83\uC740 Python\uC774\
  \uB098 JavaScript\uC640 \uAC19\uC740 \uC5B8\uC5B4\uB9CC\uD07C \uC9C1\uAD00\uC801\
  \uC774\uC9C0 \uC54A\uC744 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uB300\uD654\uD615 \uC178(REPL) \uC0AC\uC6A9\uD558\uAE30"
weight: 34
---

## 어떻게:
C REPL과 상호작용하는 것은 Python이나 JavaScript와 같은 언어만큼 직관적이지 않을 수 있습니다. 그러나 Clang과 LLVM 기술을 기반으로 한 C/C++ 인터프리터인 `Cling`과 같은 도구로 가능해집니다. 시작하는 방법은 다음과 같습니다:

1. **Cling 설치**: 운영 체제에 따라 패키지 관리자에서 Cling을 찾거나 소스에서 빌드해야할 수 있습니다. 예를 들어, Ubuntu에서는 `sudo apt-get install cling`으로 간단히 설치할 수 있습니다.

2. **Cling 시작**: 터미널을 열고 `cling`을 입력하여 인터랙티브 셸을 시작합니다.

```bash
$ cling
```

3. **코드 작성**: 이제 셸에 C 코드를 직접 입력하고 즉시 결과를 볼 수 있습니다. 다음은 간단한 예제입니다:

```c
[cling]$ #include <stdio.h>
[cling]$ printf("Hello, REPL world!\n");
Hello, REPL world!
```

4. **변수와 연산 예제**: 변수를 실험하고 즉각적인 피드백을 봅니다.

```c
[cling]$ int a = 5;
[cling]$ int b = 3;
[cling]$ printf("%d + %d = %d\n", a, b, a+b);
5 + 3 = 8
```

5. **라이브러리 포함**: Cling을 사용하면 즉석에서 라이브러리를 포함할 수 있으므로, 다양한 C 기능을 활성화할 수 있습니다.

```c
[cling]$ #include <math.h>
[cling]$ printf("Square root of %f is %f\n", 4.0, sqrt(4.0));
Square root of 4.000000 is 2.000000
```

## 심층 탐구:
REPL 환경의 시작은 1960년대 Lisp로 거슬러 올라가며, 대화형 코드 평가를 지원하도록 설계되었습니다. 그러나 C의 정적이고 컴파일된 특성은 코드 실행 조정의 즉각성을 실현하는 데 어려움을 초래했습니다. Cling과 다른 C/C++ 인터프리터의 개발은 정적으로 타입된 언어에 동적 평가를 통합하는 쪽으로의 중요한 진보를 표시합니다.

특히, Cling과 같은 인터프리터를 사용하는 것은 최적화와 실행에서의 차이로 인해 컴파일된 C 코드의 행동을 완벽하게 반영하지는 않을 수 있습니다. 또한, 교육 목적, 신속한 프로토타이핑 및 디버깅에 매우 유용하지만, REPL은 전통적인 컴파일-실행-디버그 주기에 비해 때때로 생산 수준의 코드 개발에 비해 느리고 실용적이지 않을 수 있습니다.

대화형 C 프로그래밍을 위한 대안으로는 작은 자체 포함 프로그램을 작성하고 통합 디버깅 도구가 있는 견고한 IDE를 사용하는 것이 있으며, 이는 실행에 대한 더 많은 제어와 통찰력을 제공할 수 있지만, 즉각성이 덜합니다. 이러한 대안에도 불구하고, C에서 REPL 환경의 등장은 언어의 다재다능성을 확장하는 흥미로운 발전을 나타내며, 개발 주기에서 유연성과 속도에 대한 현대적인 요구를 포용합니다.
