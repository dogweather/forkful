---
title:                "C#: 编写 测试"
simple_title:         "编写 测试"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么

无论是编写一个小型的项目还是一个大型的软件，编写测试代码都是非常重要的。它可以帮助我们发现潜在的错误并提高代码的可靠性。通过测试代码，我们可以更容易地修改和维护代码，并且可以提供一定程度的保障，让我们在代码变更后仍然可以保持一致的功能。

## 如何做

在C#中，编写测试代码的方式非常简单。首先，我们需要创建一个测试项目，然后在项目中添加我们要测试的代码。编写测试代码时，需要遵循一定的模式：AAA模式（Arrange，Act，Assert）。首先，我们安排测试的环境，接着进行操作，最后检查测试结果是否满足我们的预期。

让我们来看一个例子，在这个例子中，我们将测试一个简单的计算器程序，检查它的加法功能是否正确。

```C#
// 安排
int x = 10;
int y = 5;
int expected = 15;
Calculator calc = new Calculator();

// 操作
int actual = calc.Add(x, y);

// 断言
Assert.AreEqual(expected, actual);
```

上面的代码中，我们首先安排了测试所需的环境，即创建了两个整数和一个计算器对象。然后进行加法运算，并且通过断言来检查运算的结果是否与预期相符。

除了单元测试，我们还可以编写集成测试和UI测试，来测试整个项目的功能是否正常。

## 深入探讨

编写测试代码并非只是为了通过测试或者提高代码的可靠性。它也可以帮助我们在开发过程中不断思考代码的设计和结构。当我们考虑如何编写测试代码时，也在间接地思考如何更好地编写生产代码。

此外，编写测试代码也有助于我们减少回归错误。当我们对已有的代码进行修改时，如果没有相应的测试代码，那么可能会导致其他地方的功能受到影响。而有了测试代码，我们可以快速地检查是否有其他功能受到影响，从而减少回归错误的发生。

## 参考资源

- [NUnit官方文档](https://nunit.org/)
- [C#中编写测试的最佳实践](https://docs.microsoft.com/en-us/dotnet/core/testing/index)
- [使用Visual Studio编写测试](https://docs.microsoft.com/en-us/visualstudio/test/writing-unit-tests-for-cpp?view=vs-2022)
- [关于单元测试的更多信息](https://www.makeuseof.com/what-is-unit-testing-learn/)

## 参见

- [AAA模式](https://en.wikipedia.org/wiki/Arrange%E2%80%93act%E2%80%93assert)
- [测试驱动开发](https://en.wikipedia.org/wiki/Test-driven_development)
- [持续集成](https://www.atlassian.com/continuous-delivery/continuous-integration)