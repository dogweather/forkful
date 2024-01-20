---
title:                "编写测试代码"
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)

编写测试是创建检查代码正确性的程序。程序员进行测试以避免bug，保证代码质量，和简化维护。

## How to (如何操作)

在C#中，我们使用 `NUnit` 或 `xUnit` 等测试框架。以下是一个简单的 `NUnit` 测试示例。

```C#
using NUnit.Framework;

namespace MyApplication.Tests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_TwoNumbers_ReturnsSum()
        {
            // Arrange
            var calculator = new Calculator();

            // Act
            var result = calculator.Add(5, 7);

            // Assert
            Assert.AreEqual(12, result);
        }
    }

    public class Calculator
    {
        public int Add(int a, int b)
        {
            return a + b;
        }
    }
}
```

运行以上测试，如果代码正确，测试应该通过无输出错误。

## Deep Dive (深入了解)

C#测试编写的历史可以追溯到NUnit的推出。NUnit是一个自xUnit开始，广泛使用的测试框架。替代品包括MSTest和xUnit，每个都有其独特功能。在实现上，测试应该遵循Arrange-Act-Assert模式，这有助于保持代码清晰和有组织。

## See Also (另请参阅)

- NUnit 官方网站: [https://nunit.org/](https://nunit.org/)
- xUnit 官方网站: [https://xunit.net/](https://xunit.net/)
- MSTest 文档: [https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-mstest](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-mstest)