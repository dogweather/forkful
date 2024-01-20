---
title:                "Writing tests"
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests in programming means creating mini-programs to check if your code works as expected. Programmers do it to catch bugs, ensure quality, and save time fixing issues later.

## How to:
Let's dive into some C# code using NUnit, a popular testing framework:

1. Set up your testing framework - typically included as a NuGet package.
2. Write a test for a simple function.

Here's a quick example of a test for a `Sum` method:

```C#
using NUnit.Framework;

namespace CalculatorTests {
    public class Calculator {
        public int Sum(int a, int b) {
            return a + b;
        }
    }

    [TestFixture]
    public class CalculatorTests {
        [Test]
        public void TestSum() {
            var calculator = new Calculator();
            var result = calculator.Sum(2, 3);
            Assert.AreEqual(5, result);
        }
    }
}
```

Run the test. If it passes, you'll see:

```
Test Passed
```

Otherwise, you'll get details on why it failed.

## Deep Dive
Unit testing has evolved since the 1970s. Notable advances include test-driven development and automated testing frameworks. For C#, MSTest and xUnit are solid alternatives to NUnit. Key points include:

1. **Historical Context**: Kent Beck, among others, developed the xUnit architecture that underpins many frameworks.
2. **Alternatives**: MSTest is Microsoft's native test framework, while xUnit is a free, open-source tool.
3. **Implementation Details**: Tests should be isolated, repeatable, and fast. Run them as part of your build process.

## See Also
- [NUnit Documentation](https://docs.nunit.org/)
- [Microsoft Testing Overview](https://docs.microsoft.com/en-us/dotnet/core/testing/)
- [xUnit GitHub](https://github.com/xunit/xunit)