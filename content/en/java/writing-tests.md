---
title:    "Java recipe: Writing tests"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/writing-tests.md"
---

{{< edit_this_page >}}

## Why 
Writing tests may seem like an extra step in the software development process, but in the long run, it can save you time and headaches. By writing tests, you can catch bugs and errors before they escalate into larger problems. It also ensures that your code is functioning as intended, making it easier to maintain and update in the future.

## How To 
Writing tests in Java is a relatively simple process. The first step is to create a separate folder for your tests, preferably named "tests". Inside this folder, create a file for each class you want to test. For example, if you have a class called "Calculator", you would create a "CalculatorTest" file. 

Within the file, you will need to import the necessary testing libraries, such as JUnit or TestNG. These libraries provide the tools for creating and running tests. 

Next, you will need to create test methods for each method in your class. These methods should have a clear and descriptive name, such as "testAddition" or "testSubtraction". Within these methods, you will write code to set up the necessary test conditions and then assert the expected output using the "assertEquals" method. 

Let's take a closer look at an example of testing the "add" method in our "Calculator" class:

```Java
@Test
public void testAddition() {
  Calculator calculator = new Calculator();
  int result = calculator.add(2, 3);
  assertEquals(5, result);
}
```

In this code block, we first annotate the method with "@Test", indicating that it is a test method. Then, we create an instance of our "Calculator" class and use the "add" method to calculate the sum of 2 and 3. Finally, we use the "assertEquals" method to compare the expected result (5) with the actual result. 

Once you have written tests for all methods in your class, you can run the tests by right-clicking on the test file and selecting "Run as JUnit test". This will run all the test methods and provide you with a report of which tests passed and failed. 

## Deep Dive 
Writing tests not only helps catch bugs, but it also promotes better coding practices. By breaking down your code into smaller, testable methods, you are implementing the principle of separation of concerns. This makes it easier to debug and modify your code in the future. 

Another important aspect of testing is code coverage. This measures the percentage of your code that is covered by tests. Ideally, you want to aim for a high code coverage to ensure that all parts of your code are being tested. 

There are also different types of tests you can write, such as unit tests, integration tests, and end-to-end tests, each serving a specific purpose. By utilizing all types of tests, you can ensure that your software is thoroughly tested and functioning as expected. 

## See Also 
- [JUnit documentation](https://junit.org/junit5/docs/current/user-guide/)
- [TestNG documentation](https://testng.org/doc/)
- [Code coverage tools for Java](https://dzone.com/articles/code-coverage-tools-for-java)
- [Types of software testing](https://www.softwaretestinghelp.com/types-of-software-testing/)