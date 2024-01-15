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

## Why

Writing tests may seem like an extra step in the development process, but it serves an important purpose. By writing tests, you can catch bugs and errors in your code early on, saving you time and effort in the long run. Not only does it ensure the functionality of your code, but it also improves the overall quality and maintainability of your codebase.

## How To

To start writing tests in C#, you will need to use a testing framework such as NUnit, xUnit, or MSTest. These frameworks provide useful tools and methods for creating and running tests. Let's take a look at an example using NUnit:

```C#
[Test]
public void SquareNumber_InputFive_ReturnsTwentyFive()
{
    //Arrange
    int num = 5;
    int expected = 25;

    //Act
    int result = num * num;

    //Assert
    Assert.AreEqual(expected, result);
}
```

In this example, we are testing a function that squares a given number. Within the `[Test]` attribute, we provide a descriptive name for the test. In the `Arrange` section, we set up the necessary variables and inputs. Then, in the `Act` section, we execute the function we want to test. Finally, in the `Assert` section, we verify if the expected result matches the actual result. This test will pass since the expected result of squaring 5 is 25.

## Deep Dive

When it comes to writing tests, it is important to keep in mind the principles of good testing. Test cases should be designed to cover different scenarios and edge cases. It's also important to have a good balance between unit, integration, and end-to-end tests.

Another helpful tip is to follow the AAA pattern when writing tests - Arrange, Act, Assert. This keeps the tests organized and easy to read. Additionally, it's a good practice to run tests frequently and in an automated fashion to catch any bugs early on.

See Also
- [Introduction to Unit Testing in C#](https://docs.microsoft.com/en-us/dotnet/core/testing/introduction-to-unit-testing-in-csharp)
- [The Art of Writing Unit Tests](https://blog.jetbrains.com/dotnet/2019/08/05/art-writing-unit-tests-better-tests-learn-write-tests-right-way/)
- [Good Unit Tests: A Basic Guide](https://blog.gurock.com/good-unit-tests-basic-guide)