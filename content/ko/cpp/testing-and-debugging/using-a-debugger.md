---
title:                "디버거 사용하기"
aliases: - /ko/cpp/using-a-debugger.md
date:                  2024-01-26T03:48:03.988906-07:00
model:                 gpt-4-0125-preview
simple_title:         "디버거 사용하기"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/using-a-debugger.md"
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
