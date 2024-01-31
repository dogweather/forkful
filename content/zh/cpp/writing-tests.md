---
title:                "编写测试代码"
date:                  2024-01-19
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (测试编写是什么？为什么要做？)
编写测试是创建代码片段来检查软件的功能是否按预期工作。程序员这样做来确保修改或新增功能不会破坏现有系统。

## How to: (如何进行：)
下面用C++编写一个简单的测试用例例子。我们将使用Google Test框架。

```C++
#include <gtest/gtest.h>

int Add(int a, int b) {
    return a + b;
}

TEST(AdditionTest, HandlesPositiveNumbers) {
    EXPECT_EQ(Add(1, 2), 3);
}

TEST(AdditionTest, HandlesNegativeNumbers) {
    EXPECT_EQ(Add(-1, -1), -2);
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

当运行上述代码时，你应该看到以下输出表示测试通过：

```
[==========] Running 2 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 2 tests from AdditionTest
[ RUN      ] AdditionTest.HandlesPositiveNumbers
[       OK ] AdditionTest.HandlesPositiveNumbers (0 ms)
[ RUN      ] AdditionTest.HandlesNegativeNumbers
[       OK ] AdditionTest.HandlesNegativeNumbers (0 ms)
[----------] 2 tests from AdditionTest (0 ms total)

[----------] Global test environment tear-down
[==========] 2 tests from 1 test suite ran. (1 ms total)
[  PASSED  ] 2 tests.
```

## Deep Dive (深入探讨)
早期，C++测试不像现在这样简单。现在有多个测试框架可用（如Google Test, Boost.Test, Catch2等），大大简化了测试实施工作。不同框架有各自的优点：例如，Google Test适合大型项目，Catch2更易用于头文件。你的选择可能取决于项目需求、团队偏好和框架特性。

## See Also (另请参阅)
- Google Test官方文档: https://google.github.io/googletest/
- Boost.Test文档: https://www.boost.org/doc/libs/release/libs/test/
- Catch2文档: https://github.com/catchorg/Catch2
