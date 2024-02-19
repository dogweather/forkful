---
aliases:
- /ko/cpp/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:17.734113-07:00
description: "C++\uC5D0\uC11C \uD14C\uC2A4\uD2B8 \uC791\uC131\uC740 \uCF54\uB4DC\uBCA0\
  \uC774\uC2A4\uC758 \uC139\uC158\uB4E4\uC758 \uD589\uB3D9\uC744 \uC790\uB3D9\uC73C\
  \uB85C \uAC80\uC99D\uD558\uB294 \uC791\uACE0 \uB3C5\uB9BD\uC801\uC778 \uD504\uB85C\
  \uADF8\uB7A8\uC744 \uB9CC\uB4DC\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774 \uC791\uC5C5\uC744 \uCF54\uB4DC\
  \uAC00 \uC608\uC0C1\uB300\uB85C \uC791\uB3D9\uD558\uB294\uC9C0 \uD655\uC2E4\uD788\
  \ \uD558\uACE0, \uD68C\uADC0(\uC989, \uC0C8\uB85C\uC6B4 \uBCC0\uACBD\uC0AC\uD56D\
  \uC774 \uAE30\uC874 \uAE30\uB2A5\uC744 \uB9DD\uAC00\uB728\uB9AC\uB294 \uAC83)\uB97C\
  \ \uBC29\uC9C0\uD558\uBA70, \uC2DC\uAC04\uC774 \uC9C0\uB098\uB3C4\u2026"
lastmod: 2024-02-18 23:09:06.688657
model: gpt-4-0125-preview
summary: "C++\uC5D0\uC11C \uD14C\uC2A4\uD2B8 \uC791\uC131\uC740 \uCF54\uB4DC\uBCA0\
  \uC774\uC2A4\uC758 \uC139\uC158\uB4E4\uC758 \uD589\uB3D9\uC744 \uC790\uB3D9\uC73C\
  \uB85C \uAC80\uC99D\uD558\uB294 \uC791\uACE0 \uB3C5\uB9BD\uC801\uC778 \uD504\uB85C\
  \uADF8\uB7A8\uC744 \uB9CC\uB4DC\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774 \uC791\uC5C5\uC744 \uCF54\uB4DC\
  \uAC00 \uC608\uC0C1\uB300\uB85C \uC791\uB3D9\uD558\uB294\uC9C0 \uD655\uC2E4\uD788\
  \ \uD558\uACE0, \uD68C\uADC0(\uC989, \uC0C8\uB85C\uC6B4 \uBCC0\uACBD\uC0AC\uD56D\
  \uC774 \uAE30\uC874 \uAE30\uB2A5\uC744 \uB9DD\uAC00\uB728\uB9AC\uB294 \uAC83)\uB97C\
  \ \uBC29\uC9C0\uD558\uBA70, \uC2DC\uAC04\uC774 \uC9C0\uB098\uB3C4\u2026"
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

C++에서 테스트 작성은 코드베이스의 섹션들의 행동을 자동으로 검증하는 작고 독립적인 프로그램을 만드는 것을 포함합니다. 프로그래머들은 이 작업을 코드가 예상대로 작동하는지 확실히 하고, 회귀(즉, 새로운 변경사항이 기존 기능을 망가뜨리는 것)를 방지하며, 시간이 지나도 유지보수 가능한 코드베이스를 촉진하기 위해 이를 실시합니다.

## 방법:

### Google Test 프레임워크 사용하기

C++에서 테스트를 작성하기 위한 가장 인기 있는 제3자 라이브러리 중 하나는 Google Test입니다. 먼저, Google Test를 설치하고 프로젝트와 연결해야 합니다. 설정이 완료되면, 테스트 케이스 작성을 시작할 수 있습니다.

```cpp
#include <gtest/gtest.h>

int add(int a, int b) {
    return a + b;
}

TEST(TestSuiteName, TestName) {
    EXPECT_EQ(3, add(1, 2));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

코드를 파일에 저장하고, g++ 컴파일러를 사용하여 Google Test 라이브러리와 연결하여 컴파일합니다. 모든 설정이 정확히 이루어진다면, 결과 실행 파일을 실행하면 테스트가 실행되고, `add` 함수가 예상대로 작동한다면 다음과 같은 것을 볼 수 있습니다:

```
[==========] 1개의 테스트 스위트에서 1개의 테스트가 실행되었습니다.
[----------] 글로벌 테스트 환경 설정.
[----------] TestSuiteName에서 1개의 테스트
[ RUN      ] TestSuiteName.TestName
[       OK ] TestSuiteName.TestName (0 ms)
[----------] TestSuiteName에서 1개의 테스트 (총 0 ms)

[==========] 1개의 테스트 스위트에서 1개의 테스트가 실행되었습니다. (총 1 ms)
[  PASSED  ] 1개의 테스트.
```

### Catch2 사용하기

C++을 위한 또 다른 인기 있는 테스트 프레임워크는 Catch2입니다. 이는 더 간단한 문법을 가지고 있으며, 보통 라이브러리에 대한 링크가 필요 없습니다(헤더 전용). Catch2를 사용하여 간단한 테스트를 작성하는 예는 다음과 같습니다:

```cpp
#define CATCH_CONFIG_MAIN  // 이것은 Catch에게 main()을 제공하라고 알립니다 - 이것은 하나의 cpp 파일에서만 수행하세요
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "정수는 곱해진다", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

이 테스트를 컴파일하고 실행하면, Catch2는 테스트가 통과했는지 실패했는지를 나타내는 명확한 출력을 제공하며, 실패를 디버깅하는데 필요한 정보를 제공합니다:

```
===============================================================================
모든 테스트가 통과했습니다 (1개의 테스트 케이스에서 1개의 주장)
```

이 예시들은 테스트 프레임워크를 C++ 개발 워크플로우에 통합하는 것이 코드의 신뢰성과 유지보수 가능성을 크게 향상시킬 수 있음을 보여줍니다.
