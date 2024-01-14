---
title:    "C#: 编写测试"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

# 为什么要编写测试

在软件开发中，编写测试是一个非常重要的环节。它可以帮助我们发现代码中潜在的错误，更好地理解代码功能，并且可以保证功能的稳定性和可靠性。虽然编写测试可能会花费些许额外的时间和精力，但是它可以节省更多的时间和精力在后期修复由未经测试的代码导致的问题。

## 如何编写测试

首先，让我们来学习如何使用C#编写测试。下面是一个简单的例子，用来测试一个加法函数：

```C#
public int Add(int a, int b)
{
    return a + b;
}
```

现在，我们可以编写一个测试来验证这个函数是否按照预期工作。我们可以使用C#的单元测试框架xUnit来编写测试代码：

```C#
[Fact]
public void TestAdd()
{
    int result = Add(3, 5);
    Assert.Equal(8, result);
}
```

在上面的代码中，我们使用xUnit中的`Fact`特性来标记这个测试方法，然后调用我们要测试的加法函数并进行断言。运行这个测试，如果加法函数返回的结果与我们预期的相同，测试就会通过。否则，就会抛出断言错误。

## 深入了解测试

编写测试不仅仅是为了验证代码是否按照预期工作，它还可以帮助我们改善代码质量。通过编写测试，我们可以更早地发现潜在的错误，并且在开发过程中持续保障代码的可靠性。相比手动测试，自动化测试可以更有效地发现问题，并且可以随时运行来验证代码的正确性。

另外，编写测试还可以促使我们编写可测试性更强的代码。即使是良好的代码也不一定是可测试的，编写测试可以让我们更深入地思考代码架构和设计，从而提高代码的可维护性和可扩展性。

总之，编写测试是一项非常重要的软件开发技能。它可以增加代码的可靠性和可测试性，为我们的代码提供更好的保障。所以，无论是在工作中还是在个人项目中，我们都应该学习并且始终坚持编写测试。

# 更多学习资源

- [xUnit官方文档](https://xunit.net/)
- [单元测试教程](https://www.tutorialspoint.com/csharp/csharp_unit_testing.htm)
- [写好单元测试的13个技巧](https://www.guru99.com/unit-testing-guide.html)

## 参考链接

- [Why Writing Tests Matters](https://www.codemag.com/Article/1906051/Big-Why-Tests-Matter-in-Software-Development)
- [The Importance of Writing Automated Tests](https://www.pluralsight.com/blog/software-development/the-importance-of-writing-automated-tests)
- [The Benefits of Writing Unit Tests](https://medium.com/@rginferno10/the-benefits-of-writing-unit-tests-9308fdcb21b4)