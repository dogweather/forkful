---
title:    "C# recipe: Writing tests"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

# Why Write Tests

Writing tests for your code may seem like an extra step that can be easily skipped, but it actually plays a crucial role in the development process. By testing your code, you can identify and fix potential bugs early on, ensuring a more stable and reliable product. Not only does it save time and effort in the long run, but it also boosts confidence in your code and gives you peace of mind knowing that it is functioning correctly.

# How To Write Tests in C#

Writing tests in C# is actually quite simple. The first step is to create a new project in your preferred development environment. Then, import a testing framework such as NUnit or xUnit to your project. These frameworks provide functionality for creating and running tests.

Next, create a new class for your tests and add the `[TestFixture]` attribute to it. This signifies to the testing framework that this class contains tests. Within this class, you can create methods with the `[Test]` attribute, which will be executed as individual tests. Then, use `Assert` statements to verify that your code is producing the expected results.

Here's an example of a simple test using NUnit:

```C#
[TestFixture]
public class CalculatorTests
{
    [Test]
    public void Add_TwoPositiveNumbers_ReturnsCorrectSum()
    {
        // Arrange (set up test data)
        int num1 = 5;
        int num2 = 10;

        // Act (perform the action to be tested)
        int result = Calculator.Add(num1, num2);

        // Assert (verify expected outcome)
        Assert.AreEqual(15, result);
    }
}
```

In this example, we are testing a `Calculator` class with an `Add` method. We set up two positive numbers, perform the addition with the `Add` method, and then verify that the result is equal to the expected sum of 15.

# Deep Dive into Writing Tests

Writing tests not only ensures the functionality of your code but also promotes good coding practices. It forces you to think about edge cases and handle potential errors, which ultimately leads to writing more robust and maintainable code.

Additionally, automated tests can be run repeatedly and quickly, giving you immediate feedback on the state of your code. This allows for easier refactoring and catch any new bugs that may arise as you make changes to your code.

Some best practices for writing tests include writing small and specific tests, using descriptive test names, and using mock objects to isolate dependencies. It's also important to regularly review and update tests as your code evolves to ensure their validity.

# See Also

- [Getting Started with NUnit and C#](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-nunit)
- [Writing Unit Tests in C# with xUnit](https://medium.com/@marcindziekuje/writing-unit-tests-in-c-with-xunit-213f078c6259)
- [Mocking in C# with Moq](https://www.pluralsight.com/guides/mocking-c-sharp-with-moq)