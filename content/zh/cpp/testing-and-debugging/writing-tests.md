---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:02.971404-07:00
description: "\u7528C++\u7F16\u5199\u6D4B\u8BD5\u5305\u62EC\u521B\u5EFA\u5C0F\u578B\
  \u7684\u3001\u72EC\u7ACB\u7684\u7A0B\u5E8F\uFF0C\u8FD9\u4E9B\u7A0B\u5E8F\u80FD\u81EA\
  \u52A8\u9A8C\u8BC1\u4F60\u7684\u4EE3\u7801\u5E93\u4E2D\u5404\u90E8\u5206\u7684\u884C\
  \u4E3A\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u786E\u4FDD\u4ED6\
  \u4EEC\u7684\u4EE3\u7801\u6309\u9884\u671F\u5DE5\u4F5C\uFF0C\u9632\u6B62\u56DE\u5F52\
  \uFF08\u5373\uFF0C\u65B0\u7684\u66F4\u6539\u7834\u574F\u73B0\u6709\u529F\u80FD\uFF09\
  \u4EE5\u53CA\u968F\u65F6\u95F4\u63A8\u79FB\u4FC3\u8FDB\u4EE3\u7801\u5E93\u7684\u53EF\
  \u7EF4\u62A4\u6027\u3002"
lastmod: '2024-03-13T22:44:48.114171-06:00'
model: gpt-4-0125-preview
summary: "\u7528C++\u7F16\u5199\u6D4B\u8BD5\u5305\u62EC\u521B\u5EFA\u5C0F\u578B\u7684\
  \u3001\u72EC\u7ACB\u7684\u7A0B\u5E8F\uFF0C\u8FD9\u4E9B\u7A0B\u5E8F\u80FD\u81EA\u52A8\
  \u9A8C\u8BC1\u4F60\u7684\u4EE3\u7801\u5E93\u4E2D\u5404\u90E8\u5206\u7684\u884C\u4E3A\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u786E\u4FDD\u4ED6\u4EEC\
  \u7684\u4EE3\u7801\u6309\u9884\u671F\u5DE5\u4F5C\uFF0C\u9632\u6B62\u56DE\u5F52\uFF08\
  \u5373\uFF0C\u65B0\u7684\u66F4\u6539\u7834\u574F\u73B0\u6709\u529F\u80FD\uFF09\u4EE5\
  \u53CA\u968F\u65F6\u95F4\u63A8\u79FB\u4FC3\u8FDB\u4EE3\u7801\u5E93\u7684\u53EF\u7EF4\
  \u62A4\u6027\u3002"
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

## 何为测试以及为什么要写测试？

用C++编写测试包括创建小型的、独立的程序，这些程序能自动验证你的代码库中各部分的行为。程序员这样做是为了确保他们的代码按预期工作，防止回归（即，新的更改破坏现有功能）以及随时间推移促进代码库的可维护性。

## 如何写测试：

### 使用Google测试框架

对于用C++编写测试，最受欢迎的第三方库之一是Google Test。首先，你需要安装Google Test并将其与你的项目链接。一旦设置完成，你就可以开始编写测试用例了。

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

将代码保存在一个文件中，并使用g++编译器编译它，链接Google测试库。如果一切设置正确，运行结果可执行文件将运行测试，如果`add`函数按预期工作，你将看到如下内容：

```
[==========] 运行1个测试，来自1个测试套件。
[----------] 全局测试环境设置。
[----------] 1个测试来自TestSuiteName
[ 运行      ] TestSuiteName.TestName
[       OK ] TestSuiteName.TestName (0 ms)
[----------] TestSuiteName来自1个测试 (总共0 ms)

[==========] 1个测试套件的1个测试已运行。 (总共1 ms)
[ 通过了  ] 1个测试。
```

### 使用Catch2

C++的另一个流行测试框架是Catch2。它有一个更简单的语法，并且通常不需要链接库（仅头文件）。这是如何使用Catch2编写一个简单测试的示例：

```cpp
#define CATCH_CONFIG_MAIN  // 这告诉Catch提供一个main() - 只在一个cpp文件中这样做
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "整数相乘", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

编译并运行这个测试后，Catch2提供清晰的输出，指示测试是通过还是失败，以及调试失败所需的任何信息：

```
===============================================================================
所有测试都已通过 (1个断言在1个测试案例中)
```

这些例子展示了如何将测试框架集成到你的C++开发工作流中，可以显著提高你的代码的可靠性和可维护性。
