---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:27.718385-07:00
description: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178, \uB610\uB294 \uC77D\uAE30-\uD3C9\
  \uAC00-\uCD9C\uB825 \uB8E8\uD504(REPL)\uB85C \uC54C\uB824\uC9C4 \uC774 \uAE30\uC220\
  \uC740 \uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uD45C\uD604\uC2DD\uC774\uB098 \uCF54\
  \uB4DC\uB97C \uC785\uB825\uD558\uACE0 \uC989\uC2DC \uACB0\uACFC\uB97C \uBCFC \uC218\
  \ \uC788\uAC8C \uD568\uC73C\uB85C\uC368 \uD559\uC2B5\uACFC \uB514\uBC84\uAE45 \uACFC\
  \uC815\uC744 \uD5A5\uC0C1\uC2DC\uD0B5\uB2C8\uB2E4. C\uAC00 \uC804\uD1B5\uC801\uC73C\
  \uB85C REPL \uD658\uACBD\uC744 \uAE30\uBCF8\uC801\uC73C\uB85C \uC9C0\uC6D0\uD558\
  \uC9C0 \uC54A\uC74C\uC5D0\uB3C4 \uBD88\uAD6C\uD558\uACE0, \uD604\uB300\uC758 \uB3C4\
  \uAD6C\uB4E4\uC740 \uC774\uB7EC\uD55C\u2026"
lastmod: '2024-03-11T00:14:29.851483-06:00'
model: gpt-4-0125-preview
summary: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178, \uB610\uB294 \uC77D\uAE30-\uD3C9\uAC00\
  -\uCD9C\uB825 \uB8E8\uD504(REPL)\uB85C \uC54C\uB824\uC9C4 \uC774 \uAE30\uC220\uC740\
  \ \uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uD45C\uD604\uC2DD\uC774\uB098 \uCF54\uB4DC\
  \uB97C \uC785\uB825\uD558\uACE0 \uC989\uC2DC \uACB0\uACFC\uB97C \uBCFC \uC218 \uC788\
  \uAC8C \uD568\uC73C\uB85C\uC368 \uD559\uC2B5\uACFC \uB514\uBC84\uAE45 \uACFC\uC815\
  \uC744 \uD5A5\uC0C1\uC2DC\uD0B5\uB2C8\uB2E4. C\uAC00 \uC804\uD1B5\uC801\uC73C\uB85C\
  \ REPL \uD658\uACBD\uC744 \uAE30\uBCF8\uC801\uC73C\uB85C \uC9C0\uC6D0\uD558\uC9C0\
  \ \uC54A\uC74C\uC5D0\uB3C4 \uBD88\uAD6C\uD558\uACE0, \uD604\uB300\uC758 \uB3C4\uAD6C\
  \uB4E4\uC740 \uC774\uB7EC\uD55C\u2026"
title: "\uB300\uD654\uD615 \uC178(REPL) \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜?

인터랙티브 셸, 또는 읽기-평가-출력 루프(REPL)로 알려진 이 기술은 프로그래머가 표현식이나 코드를 입력하고 즉시 결과를 볼 수 있게 함으로써 학습과 디버깅 과정을 향상시킵니다. C가 전통적으로 REPL 환경을 기본적으로 지원하지 않음에도 불구하고, 현대의 도구들은 이러한 격차를 메우며, C 프로그램의 동적 탐색을 제공합니다.

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
