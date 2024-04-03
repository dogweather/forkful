---
date: 2024-01-26 01:17:29.002137-07:00
description: "\uC5B4\uB5BB\uAC8C: \uAC1D\uCCB4\uB97C \uCD08\uAE30\uD654\uD558\uBA74\
  \uC11C \uB85C\uAE45\uB3C4 \uC218\uD589\uD558\uB294 \uB4F1 \uB108\uBB34 \uB9CE\uC740\
  \ \uC77C\uC744 \uD558\uACE0 \uC788\uB294 \uD568\uC218\uAC00 \uC788\uB2E4\uACE0 \uAC00\
  \uC815\uD574 \uBCF4\uC138\uC694. \uB2E4\uC74C\uACFC \uAC19\uC774 \uB454\uD0C1\uD55C\
  \ \uBA54\uC11C\uB4DC\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.681119-06:00'
model: gpt-4-0125-preview
summary: "\uAC1D\uCCB4\uB97C \uCD08\uAE30\uD654\uD558\uBA74\uC11C \uB85C\uAE45\uB3C4\
  \ \uC218\uD589\uD558\uB294 \uB4F1 \uB108\uBB34 \uB9CE\uC740 \uC77C\uC744 \uD558\uACE0\
  \ \uC788\uB294 \uD568\uC218\uAC00 \uC788\uB2E4\uACE0 \uAC00\uC815\uD574 \uBCF4\uC138\
  \uC694."
title: "\uB9AC\uD329\uD1A0\uB9C1"
weight: 19
---

## 어떻게:
객체를 초기화하면서 로깅도 수행하는 등 너무 많은 일을 하고 있는 함수가 있다고 가정해 보세요. 다음과 같이 둔탁한 메서드입니다:

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // 초기화 로직
        // ...

        // 상세 로깅
        if (verbose) {
            std::cout << "위젯 초기화됨!" << std::endl;
        }
    }
};

// 사용법:
Widget w;
w.init(true);
```

출력:
```
위젯 초기화됨!
```

이것을 더 깨끗하고, 집중된 메서드로 리팩토링하면 다음과 같습니다:

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // 오직 초기화 로직만
        // ...
    }

    void logInitialization() const {
        std::cout << "위젯 초기화됨!" << std::endl;
    }
};

// 사용법:
Widget w;
w.init();
w.logInitialization();
```

이 변경은 프로그램이 하는 일을 변경하지 않았지만, `Widget` 클래스를 더 모듈화하고 사용법을 더 명확하게 만들었습니다.

## 심층 분석
우리가 오늘날 알고 있는 리팩토링 개념의 뿌리는 1980년대 Smalltalk 프로그래밍 커뮤니티에서 시작되었으며, Martin Fowler의 1999년 책 "Refactoring: Improving the Design of Existing Code"로 인해 크게 대중화되었습니다. 오늘날, 리팩토링은 Agile 및 TDD(테스트 주도 개발)과 같은 다양한 개발 방법론에 통합된 현대 소프트웨어 개발의 핵심 부분입니다.

리팩토링의 대안에 대해 이야기할 때, 우리는 재작성이나 재설계 영역으로 나아갑니다. 리팩토링은 전략적이고 점진적인 반면, 재작성은 기존 코드를 버리고 새로운 해결책을 선호할 수 있습니다. 한편, 재설계는 기능을 변경하는 것을 포함한 더 큰 변경사항을 수반할 수 있으며, 이는 순수 리팩토링에 대한 목표가 아닙니다.

리팩토링의 구현 세부 사항은 상당히 세밀해질 수 있습니다. 긴 메서드, 큰 클래스 또는 중복된 코드와 같은 많은 '코드 냄새'가 리팩토링을 촉발할 수 있습니다. C++용 "Clang-Tidy"와 같은 자동화된 도구가 존재하여 문제를 파악하고 일부 수정을 적용할 수 있습니다.

또한, 리팩토링에는 기능성이 변경되지 않았는지 확인하기 위한 견고한 테스트 스위트가 필요합니다. 테스트 없이는 사실상 눈가림 상태로 비행하며 회귀를 위험에 빠뜨리게 됩니다.

## 참고 자료
리팩토링에 대한 더 깊은 이해와 더 많은 예제를 보려면 다음을 확인해 보세요:

- 기초적인 아이디어와 전략을 위한 Martin Fowler의 고전적인 텍스트 "Refactoring: Improving the Design of Existing Code".
- C++에서 자동 리팩토링 지원을 위한 `Clang-Tidy` 문서, https://clang.llvm.org/extra/clang-tidy/ 참조.
- 완벽하지 않은 기존 코드베이스의 맥락에서 안전하게 리팩토링을 수행하기 위한 기법을 제공하는 Michael Feathers의 "Working Effectively with Legacy Code".
