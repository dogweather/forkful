---
title:                "테스트 작성하기"
date:                  2024-01-19
simple_title:         "테스트 작성하기"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
테스트 작성은 코드가 의도한 대로 작동하는지 검증하는 과정입니다. 개발자는 버그를 줄이고, 코드 품질을 유지하며, 장기적으로 시간을 절약하기 위해 테스트를 합니다.

## How to: (방법)
C++에서 테스트를 작성하는 한 가지 표준 방법은 Google Test 프레임워크를 사용하는 것입니다. 아래 예제 코드를 보세요:

```c++
#include <gtest/gtest.h>

int Add(int a, int b) {
    return a + b;
}

TEST(AdditionTest, HandlesPositiveNumbers) {
    EXPECT_EQ(7, Add(3, 4));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```
컴파일 후 해당 테스트를 실행하면 다음과 같은 결과가 출력됩니다:

```
[==========] Running 2 tests from 1 test case.
[----------] Global test environment set-up.
[----------] 2 tests from AdditionTest
[ RUN      ] AdditionTest.HandlesPositiveNumbers
[       OK ] AdditionTest.HandlesPositiveNumbers (0 ms)
[----------] 2 tests from AdditionTest (0 ms total)

[----------] Global test environment tear-down
[==========] 2 tests from 1 test case ran. (1 ms total)
[  PASSED  ] 2 tests.
```

## Deep Dive (심층 분석)
테스트 작성 방법은 오래 전부터 중요했습니다. 초기에는 단순히 출력을 확인하는 방식이었지만, 지금은 TDD(Test-Driven Development) 같은 체계적인 접근 방식이 있습니다. Google Test는 C++의 여러 테스트 프레임워크 중 하나로, xUnit 아키텍처를 따르며 사용자에게 많은 기능을 제공합니다. 이외에도 Catch2, Boost.Test 등 다양한 대안이 존재합니다. 구현 세부 사항으로는 테스트 케이스, 테스트 스위트, 어설션 등이 있으며, 이들은 개발자가 보다 쉽게 코드를 검증할 수 있도록 돕습니다.

## See Also (참고 자료)
- [Google Test GitHub repository](https://github.com/google/googletest)
- [Google Test official documentation](https://google.github.io/googletest/)
- [Catch2 GitHub repository](https://github.com/catchorg/Catch2)
- [Boost.Test Documentation](https://www.boost.org/doc/libs/1_75_0/libs/test/doc/html/index.html)
