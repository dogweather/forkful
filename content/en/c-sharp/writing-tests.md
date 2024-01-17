---
title:                "Writing tests"
html_title:           "C# recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests is the process of creating automated checks to verify that a piece of code behaves as expected. It is an essential part of the software development process as it helps to identify potential bugs and errors in the code. By writing tests, programmers can ensure that their code is functioning correctly, leading to more efficient and reliable software.

## How to:
To write tests in C#, we use the built-in testing framework called NUnit. First, we need to create a new console project in Visual Studio. Then, we add the NUnit framework through the NuGet Package Manager. We can then create a new class for our tests and decorate it with the [TestFixture] attribute. Within this class, we can write test methods and use the [Test] attribute to indicate that these methods are tests. Finally, we can run our tests using the Test Explorer in Visual Studio.

```C#
[TestFixture]
public class CalculatorTests
{
    [Test]
    public void Add_WithPositiveNumbers_ReturnsCorrectResult()
    {
        // Arrange
        int num1 = 5;
        int num2 = 10;
        int expectedResult = 15;

        // Act
        int result = Calculator.Add(num1, num2);

        // Assert
        Assert.AreEqual(expectedResult, result);
    }
}
```

Sample output in the Test Explorer:

![Test Explorer](https://image.ibb.co/kSdRDF/test_explorer.png)

## Deep Dive:
Unit testing, the process of testing individual units or components of code, was first introduced in the 1970s with the advent of structured programming. Prior to that, integration testing, where multiple units of code were tested together, was the norm. Today, there are various testing frameworks available for different languages, but NUnit remains one of the most popular for C#.

Alternatives to NUnit in C# include Xunit and Microsoft's MSTest framework. While NUnit and Xunit are open-source and cross-platform, MSTest is only available for use within the Windows ecosystem. However, MSTest does offer features such as running tests in parallel and integrating with Azure DevOps.

NUnit supports various assertion methods such as Assert.AreEqual() and Assert.IsTrue(), which can be used to verify the expected results of a test. It also allows for setting up and tearing down test fixtures, which can be useful for setting up common data or objects needed for multiple tests.

## See Also:
- [NUnit Documentation](https://nunit.org/documentation/)
- [Introduction to Unit Testing in C#](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-nunit)
- [Xunit Documentation](https://xunit.net/)
- [MSTest Documentation](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-mstest)