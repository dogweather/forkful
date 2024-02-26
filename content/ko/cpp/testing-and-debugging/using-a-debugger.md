---
date: 2024-01-26 03:48:03.988906-07:00
description: "\uB514\uBC84\uAC70\uB97C \uC0AC\uC6A9\uD55C\uB2E4\uB294 \uAC83\uC740\
  \ \uC2E4\uC81C\uB85C \uBB34\uC2A8 \uC77C\uC774 \uC77C\uC5B4\uB098\uACE0 \uC788\uB294\
  \uC9C0 \uC774\uD574\uD558\uAE30 \uC704\uD574 \uC2E4\uD589 \uC911\uC778 \uD504\uB85C\
  \uADF8\uB7A8 \uB0B4\uBD80\uB97C \uB4E4\uC5EC\uB2E4\uBCFC \uC218 \uC788\uB294 \uB3C4\
  \uAD6C\uB97C \uAC00\uB3D9\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4\
  . \uAC1C\uBC1C\uC790\uB4E4\uC740 \uCF54\uB4DC\uAC00 \uC608\uC0C1\uCE58 \uBABB\uD558\
  \uAC8C \uB3D9\uC791\uD558\uAC70\uB098 \uCDA9\uB3CC\uC744 \uC77C\uC73C\uD0A4\uB294\
  \ \uC131\uAC00\uC2E0 \uBB38\uC81C\uB4E4, \uC989 \uBC84\uADF8\uB97C \uCC3E\uC544\uC11C\
  \ \uC81C\uAC70\uD558\uAE30 \uC704\uD574 \uC774\uB7F0 \uC791\uC5C5\uC744\u2026"
lastmod: '2024-02-25T18:49:52.660331-07:00'
model: gpt-4-0125-preview
summary: "\uB514\uBC84\uAC70\uB97C \uC0AC\uC6A9\uD55C\uB2E4\uB294 \uAC83\uC740 \uC2E4\
  \uC81C\uB85C \uBB34\uC2A8 \uC77C\uC774 \uC77C\uC5B4\uB098\uACE0 \uC788\uB294\uC9C0\
  \ \uC774\uD574\uD558\uAE30 \uC704\uD574 \uC2E4\uD589 \uC911\uC778 \uD504\uB85C\uADF8\
  \uB7A8 \uB0B4\uBD80\uB97C \uB4E4\uC5EC\uB2E4\uBCFC \uC218 \uC788\uB294 \uB3C4\uAD6C\
  \uB97C \uAC00\uB3D9\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uAC1C\
  \uBC1C\uC790\uB4E4\uC740 \uCF54\uB4DC\uAC00 \uC608\uC0C1\uCE58 \uBABB\uD558\uAC8C\
  \ \uB3D9\uC791\uD558\uAC70\uB098 \uCDA9\uB3CC\uC744 \uC77C\uC73C\uD0A4\uB294 \uC131\
  \uAC00\uC2E0 \uBB38\uC81C\uB4E4, \uC989 \uBC84\uADF8\uB97C \uCC3E\uC544\uC11C \uC81C\
  \uAC70\uD558\uAE30 \uC704\uD574 \uC774\uB7F0 \uC791\uC5C5\uC744\u2026"
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디버거를 사용한다는 것은 실제로 무슨 일이 일어나고 있는지 이해하기 위해 실행 중인 프로그램 내부를 들여다볼 수 있는 도구를 가동하는 것을 의미합니다. 개발자들은 코드가 예상치 못하게 동작하거나 충돌을 일으키는 성가신 문제들, 즉 버그를 찾아서 제거하기 위해 이런 작업을 합니다.

## 사용 방법:
C++은 GDB나 비주얼 스튜디오 디버거와 같은 디버거와 통합됩니다. 여기 GDB를 사용한 간단한 예가 있습니다:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 0;
    int c = a / b; // 이런, 0으로 나누기!
    std::cout << c << std::endl;
    return 0;
}

// 컴파일 방법:
// g++ -g -o my_program my_program.cpp

// 디버거와 함께 실행:
// gdb ./my_program
```

GDB를 시작하면, 중단점을 설정하고, 코드를 단계별로 실행하고, 변수를 검사하고, 그 외 많은 작업을 할 수 있습니다. 위의 코드를 실행하면, 0으로 나누기로 인해 프로그램이 충돌하는 것을 보게 될 것입니다.

## 심층 탐구
디버깅은 프로그래밍 초기, 하드웨어에서 직접 벌레(곤충)를 제거해야 했던 시절에 그 뿌리를 두고 있습니다. 그 이후로 디버깅 도구는 개발에 있어 필수적인 복잡하고 강력한 소프트웨어로 발전했습니다.

C++을 위한 GDB의 대안으로는 LLDB, 비주얼 스튜디오, CLion 또는 이클립스와 같은 IDE에 통합된 디버거가 있습니다. 이러한 현대적인 환경은 그래픽 인터페이스를 제공하여 디버깅을 덜 위협적으로 만듭니다.

디버거 사용에 대한 구현 세부 사항은 개발 환경에 따라 다를 수 있습니다:

- 명령줄 디버거(GDB, LLDB)는 터미널 명령과 친숙하고 종종 더 가파른 학습 곡선을 포함합니다.
- 그래픽 디버거는 중단점을 설정하고, 코드를 단계별로 실행하고, 변수를 관찰하는 등의 작업을 클릭으로 수행할 수 있게 하여 과정을 단순화합니다.

조건부 중단점, 감시점, 표현식 평가와 같은 디버거의 기능을 이해하는 것은 문제를 진단하는 효율성을 크게 향상시킬 수 있습니다.

## 참고 자료
- [GDB 문서](https://www.gnu.org/software/gdb/documentation/)
- [LLDB 명령문서](https://lldb.llvm.org/use/map.html)
- [비주얼 스튜디오 디버거 튜토리얼](https://docs.microsoft.com/ko-kr/visualstudio/debugger/debugger-feature-tour)
- [CLion으로 디버깅하기](https://www.jetbrains.com/help/clion/debugging-code.html)
