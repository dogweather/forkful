---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:04.079744-07:00
description: "\uB300\uD654\uD615 \uC258 \uB610\uB294 Read-Eval-Print Loop (REPL)\uC740\
  \ \uC0AC\uC6A9\uC790\uAC00 \uBA85\uB839\uC744 \uC785\uB825\uD558\uACE0 \uC2E4\uD589\
  \uD558\uC5EC \uC2E4\uC2DC\uAC04\uC73C\uB85C \uACB0\uACFC\uB97C \uBCFC \uC218 \uC788\
  \uAC8C \uD574\uC90D\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 REPL\uC744\
  \ \uC0AC\uC6A9\uD558\uC5EC \uBE60\uB978 \uD504\uB85C\uD1A0\uD0C0\uC774\uD551, \uCF54\
  \uB4DC \uC870\uAC01 \uD14C\uC2A4\uD2B8 \uB610\uB294 \uBCF4\uB2E4 \uC0C1\uD638 \uC791\
  \uC6A9\uC801\uC774\uACE0 \uBC18\uBCF5\uC801\uC778 \uD658\uACBD\uC5D0\uC11C \uB514\
  \uBC84\uAE45\uC744\u2026"
lastmod: 2024-02-19 22:05:13.889925
model: gpt-4-0125-preview
summary: "\uB300\uD654\uD615 \uC258 \uB610\uB294 Read-Eval-Print Loop (REPL)\uC740\
  \ \uC0AC\uC6A9\uC790\uAC00 \uBA85\uB839\uC744 \uC785\uB825\uD558\uACE0 \uC2E4\uD589\
  \uD558\uC5EC \uC2E4\uC2DC\uAC04\uC73C\uB85C \uACB0\uACFC\uB97C \uBCFC \uC218 \uC788\
  \uAC8C \uD574\uC90D\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 REPL\uC744\
  \ \uC0AC\uC6A9\uD558\uC5EC \uBE60\uB978 \uD504\uB85C\uD1A0\uD0C0\uC774\uD551, \uCF54\
  \uB4DC \uC870\uAC01 \uD14C\uC2A4\uD2B8 \uB610\uB294 \uBCF4\uB2E4 \uC0C1\uD638 \uC791\
  \uC6A9\uC801\uC774\uACE0 \uBC18\uBCF5\uC801\uC778 \uD658\uACBD\uC5D0\uC11C \uB514\
  \uBC84\uAE45\uC744\u2026"
title: "\uC778\uD130\uB799\uD2F0\uBE0C \uC178(REPL) \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?

대화형 쉘 또는 Read-Eval-Print Loop (REPL)은 사용자가 명령을 입력하고 실행하여 실시간으로 결과를 볼 수 있게 해줍니다. 프로그래머들은 REPL을 사용하여 빠른 프로토타이핑, 코드 조각 테스트 또는 보다 상호 작용적이고 반복적인 환경에서 디버깅을 수행함으로써 생산성을 높이고 코드에 대한 이해를 증진시킵니다.

## 방법:

Visual Basic for Applications (VBA) 자체는 Python이나 JavaScript와 같은 언어에서 볼 수 있는 대화형 쉘 또는 REPL 경험을 기본적으로 지원하지 않습니다. 그러나 VBA IDE(통합 개발 환경)에서 즉시 창을 사용하여 어느 정도 이러한 경험을 시뮬레이션할 수 있습니다.

**즉시 창 접근 방법:**
1. Office 애플리케이션에서 `Alt + F11`을 눌러 VBA IDE를 엽니다.
2. 즉시 창이 보이지 않는 경우 `Ctrl + G`를 누르거나 보기 메뉴에서 선택하여 열 수 있습니다.

**즉시 창을 REPL로 사용하기:**
- 코드 한 줄을 실행하려면, 단순히 즉시 창에 입력하고 Enter를 누릅니다. 예를 들어:

```basic
Debug.Print 2 + 2
```

- 샘플 출력:
```
 4
```

- 모듈에 정의된 함수와 서브루틴을 호출할 수도 있습니다:

```basic
Public Sub SayHello()
    Debug.Print "Hello, World!"
End Sub
```

- 그리고 즉시 창에서:
```basic
Call SayHello
```

- 샘플 출력:
```
 Hello, World!
```

**참고:** 즉시 창은 한계가 있습니다. 짧은 테스트와 직접적인 함수 호출에는 탁월하지만, 직접적으로 함수나 서브루틴을 정의하는 것을 지원하지 않습니다. 복잡한 디버깅과 프로그래밍 작업은 전체 모듈 개발을 요구할 수 있습니다.

## 심층 탐구

VBA의 즉시 창은 그것의 한계에도 불구하고 다른 프로그래밍 환경에서 찾을 수 있는 대화형 쉘의 가장 가까운 대응물로 작용합니다. 역사적으로, VBA는 독립 소프트웨어 개발보다는 스크립트와 매크로를 통해 Microsoft Office 애플리케이션의 기능을 확장하는 데 중점을 두었습니다. 이는 전체적인 REPL의 부재를 설명할 수 있습니다.

상호 작용적인 테스트나 복잡한 로직 개발을 요구하는 작업에 대해서는, IDLE을 갖춘 Python이나 Node.js를 이용한 JavaScript와 같이 네이티브 REPL 지원을 갖춘 다른 프로그래밍 환경이 더 나은 대안을 제공할 수 있습니다. 이 환경들은 대화형 쉘뿐만 아니라 더 강력한 프로그래밍, 디버깅, 테스팅 시설을 제공합니다.

즉시 창은 표현식을 빠르게 테스트하고, 함수를 실행하며, Office 애플리케이션 객체를 직접 조작하는 데 있어 가치 있는 도구를 제공합니다. 그런 의미에서 즉시 창은 전통적인 컴파일-실행-디버그 사이클에 비해 비교할 수 없는 즉각성과 편리함을 제공하며, VBA 개발 과정 내에서 중요한 틈새를 차지하고 있습니다. 그러나 운영 범위의 이해된 제약 사항을 가지고 있음에도 불구하고 말이죠.
