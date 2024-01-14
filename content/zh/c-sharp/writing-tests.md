---
title:                "C#: 编写测试"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么

为什么要写测试？这可能是你在编写C#代码时最常听到的问题之一。答案很简单：测试是保证代码质量的关键步骤。

写测试可以验证你的代码是否按照预期工作，帮助你发现并修复潜在的bug，从而提升整体代码质量。它也可以帮助你在将来进行重构或修改代码时，更加有信心和安全感。

## 如何做？

首先，让我们看一个简单的示例。假设我们有一个简单的函数，用于计算两个数字的和，并将结果打印出来。

```C#
int Add(int a, int b)
{
    int sum = a + b;
    Console.WriteLine($"The sum of {a} and {b} is {sum}");
    return sum;
}
```

下一步，我们可以编写相关的测试代码，检查这个函数是否按照预期工作。使用C#的内置测试框架，可以轻松实现这一点。

```C#
[TestMethod]
public void TestAdd()
{
    Assert.AreEqual(5, Add(2, 3));
}

[TestMethod]
public void TestAddNegativeNumbers()
{
    Assert.AreEqual(-10, Add(-5, -5));
}
```

在这个示例中，我们编写了两个测试用例来验证Add函数的功能。`[TestMethod]` 表示一个测试方法，而 `Assert.AreEqual` 则是断言函数，用于比较预期结果和实际结果。

现在，当我们运行这些测试时，如果一切正常，两个测试用例都会通过，这说明Add函数工作正常。

测试不仅可以用于函数，也可以用于类、接口等。在C#中，我们可以通过使用属性 `[TestClass]` 和 `[TestMethod]` 来标识测试类和测试方法。

## 深入了解

除了简单的断言函数之外，C#测试框架还提供了许多其他功能，例如 `[ExpectedException]` 属性来测试是否抛出了预期的异常，以及 `[Timeout]` 属性来设置测试方法的超时时间。

此外，还有许多第三方的测试框架可供选择，例如NUnit，xUnit等，它们提供了更多的功能和灵活性。

无论你选择哪种测试框架，重要的是要保持一致和频繁地进行测试。它可以帮助你编写高质量的代码，并在日常开发中提升你的生产力。

## 参考链接

- [Microsoft Docs: 使用C#进行单元测试](https://docs.microsoft.com/zh-cn/visualstudio/test/walkthrough-creating-and-running-unit-tests-for-managed-code?view=vs-2019)
- [C# Guide: 测试](https://docs.microsoft.com/zh-cn/dotnet/csharp/testing/)
- [NUnit官网](https://nunit.org/)
- [xUnit官网](https://xunit.net/)
- [C#单元测试示例](https://github.com/dotnet/docs/tree/master/samples/csharp/getting-started/unit-testing-using-nunit)