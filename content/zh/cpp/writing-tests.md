---
title:                "C++: 编写测试"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么要写测试
测试是软件开发过程中必不可少的一步。它可以帮助我们发现潜在的bug，确保代码的正确性，提高软件的质量。通过编写测试，我们可以更加自信地修改和添加代码，而不担心会引入新的问题。

## 如何写测试
编写测试的基本步骤如下：
1. 确定被测试的功能或模块。
2. 编写测试用例，即测试的输入和预期输出。
3. 使用单元测试框架，如Google Test或Catch2。
4. 编写测试函数，根据测试用例调用被测试的功能，并进行断言。
5. 运行测试并检查输出是否符合预期。

下面是一个简单的例子，在```C++ ```代码块中展示了如何使用Google Test框架进行断言：

```C++
#include <gtest/gtest.h>

// 被测试的函数，计算两个数的和
int add(int a, int b) {
    return a + b;
}

TEST(AddFunction, Basic) {
    // 断言：5加7应该等于12
    EXPECT_EQ(12, add(5, 7));
}

TEST(AddFunction, Zero) {
    // 断言：任何数加0都应该等于自身
    EXPECT_EQ(5, add(5, 0));
    EXPECT_EQ(0, add(0, 0));
    EXPECT_EQ(-3, add(-3, 0));
}

TEST(AddFunction, Negative) {
    // 断言：负数相加应该等于两数相加的和的相反数
    EXPECT_EQ(-8, add(-5, -3));
    EXPECT_EQ(5, add(-10, 15));
    EXPECT_EQ(0, add(-2, 2));
}

int main(int argc, char* argv[]) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

运行该测试程序，输出结果如下：

```
[==========] Running 3 tests from 3 test cases.
[----------] Global test environment set-up.
[----------] 1 test from AddFunction/Basic
[ RUN      ] AddFunction/Basic
[       OK ] AddFunction/Basic (0 ms)
[----------] 1 test from AddFunction/Basic (0 ms total)

[----------] 1 test from AddFunction/Zero
[ RUN      ] AddFunction/Zero
[       OK ] AddFunction/Zero (0 ms)
[----------] 1 test from AddFunction/Zero (0 ms total)

[----------] 1 test from AddFunction/Negative
[ RUN      ] AddFunction/Negative
[       OK ] AddFunction/Negative (0 ms)
[----------] 1 test from AddFunction/Negative (0 ms total)

[----------] Global test environment tear-down
[==========] 3 tests from 3 test cases ran. (0 ms total)
[  PASSED  ] 3 tests.
```

输出中显示所有的测试用例均通过，说明被测试的函数的行为与预期相符合。

## 深入了解测试
除了基本的断言外，测试还有很多其他的技巧和工具。例如，可以使用参数化测试来减少冗长重复的代码。还可以使用模拟框架来模拟一些外部依赖，从而方便测试。此外，测试覆盖率工具可以帮助我们确定哪些代码是未经测试的。

## 参考资料
- Google Test官方文档：https://github.com/google/googletest/blob/master/googletest/docs/Primer.md
- Catch2官方文档：https://github.com/catchorg/Catch2/blob/master/docs/tutorial.md