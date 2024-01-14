---
title:                "C++: 编写测试。"
simple_title:         "编写测试。"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么

编写测试代码是软件开发过程中很重要的一步，可以确保代码的质量和稳定性。通过编写测试，您可以及早发现和解决代码中的bug，减少后期维护成本。此外，测试可以帮助您更好地理解您的代码，并确保每次更改和添加功能不会破坏现有的功能。总而言之，编写测试可以为您的软件开发提供更高的保障，让您的代码更加可靠。

## 如何编写测试

编写测试代码可以分为三个步骤：

1. 选择合适的测试框架。测试框架是一种用于编写和运行测试的工具，如Google Test和Catch2。选择适合您项目的测试框架可以让编写测试更加简单和高效。
2. 编写测试用例。测试用例是针对您代码中不同功能和情况的测试代码。它们应该能够覆盖所有可能的场景，并验证代码是否按照预期运行。
3. 运行和分析测试结果。运行测试，并仔细分析测试结果，确保所有的测试用例都通过。如果有测试失败，您就可以找出并解决代码中的问题。

下面是一个简单的例子，演示如何使用Google Test框架来编写和运行测试：

```C++
#include <gtest/gtest.h> // 导入Google Test框架

// 定义一个简单的函数，用于计算两个数的和
int sum(int a, int b) {
    return a + b;
}

// 定义一个测试用例，使用ASSERT宏来断言测试结果是否符合预期
TEST(SumTest, PositiveNumbers) {
    ASSERT_EQ(sum(2, 2), 4); // 验证2 + 2的结果是否为4
    ASSERT_EQ(sum(5, 10), 15); // 验证5 + 10的结果是否为15
}

// 定义另一个测试用例，使用EXPECT宏来断言测试结果是否符合预期
TEST(SumTest, NegativeNumbers) {
    EXPECT_EQ(sum(-3, -5), -8); // 验证-3 + -5的结果是否为-8
    EXPECT_EQ(sum(-10, 10), 0); // 验证-10 + 10的结果是否为0
}

// 定义一个主函数，在该函数中运行所有的测试用例
int main(int argc, char* argv[]) {
    ::testing::InitGoogleTest(&argc, argv); // 初始化Google Test框架
    return RUN_ALL_TESTS(); // 运行所有的测试用例，并返回测试结果
}
```

运行以上代码，将会得到如下输出：

```
[==========] Running 2 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 2 tests from SumTest
[ RUN      ] SumTest.PositiveNumbers
[       OK ] SumTest.PositiveNumbers (0 ms)
[ RUN      ] SumTest.NegativeNumbers
[       OK ] SumTest.NegativeNumbers (0 ms)
[----------] 2 tests from SumTest (0 ms total)

[----------] Global test environment tear-down
[==========] 2 tests from 1 test suite ran. (1 ms total)
[ PASSED   ] 2 tests.
```

测试结果显示，所有的测试用例都通过了，这意味着函数sum的运行结果符合预期。

## 深入了解测试

编写测试代码还有很多更深层次的技巧，如测试覆盖率、参数化测试等。这些技巧可以帮助您更全面地测试您的代码，并提高测试的效率和可靠性。此外，熟悉测试框架的断言宏和各种命令行选项也可以帮助您更好地编写和运行测试。

## 参考资料

- [Google Test官方文