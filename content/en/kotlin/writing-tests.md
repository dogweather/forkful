---
title:    "Kotlin recipe: Writing tests"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

# Why Write Tests? 

Writing tests is an essential aspect of professional programming and plays a crucial role in ensuring the quality of code. It helps to catch bugs, identify errors early on, and save time and effort in the long run. Additionally, having a comprehensive test suite in place can improve code readability and maintainability.

## How To Write Tests in Kotlin

To write tests in Kotlin, we can use the built-in JUnit library, which is a popular testing framework for Java and other JVM languages. Let's take a look at an example of writing a simple unit test for a function that calculates the average of two numbers:

```
/**
 * Calculates the average of two numbers.
 * @param num1 The first number.
 * @param num2 The second number.
 * @return The average of the two numbers.
 */
fun average(num1: Int, num2: Int): Int {
    return (num1 + num2) / 2
}

class AverageTests {
    // Test case for a correct average calculation
    @Test
    fun testAverage() {
        assertEquals(5, average(7, 3))
    }

    // Test case for a wrong average calculation
    @Test
    fun testWrongAverage() {
        assertNotEquals(7, average(7, 3))
    }
}
```

In the above code, we have defined a function called `average` that takes in two integer parameters and returns the average of those numbers. We have also written two test cases, one for a correct average calculation and another for a wrong calculation, using the `@Test` annotation from JUnit. We can run these tests by executing the `testAverage` and `testWrongAverage` functions. If the tests pass, we will see a green checkmark, indicating the correct output. However, if a test fails, we will see a red cross along with an error message indicating the reason for the failure.

## Deep Dive: Best Practices for Writing Tests

While writing tests in Kotlin, it is essential to follow some best practices to ensure the tests are effective and efficient. Here are a few tips to keep in mind:

- Write tests for specific functionalities or methods rather than trying to test the entire codebase at once.
- Use meaningful and descriptive test names to make it easier to identify the purpose of each test.
- Use the `assertEquals` and `assertNotEquals` methods to compare expected and actual values in tests.
- Use the `@Before` and `@After` annotations to set up and clean up test data before and after each test execution.
- Write tests with both positive and negative scenarios, i.e., test for expected and unexpected inputs.
- Use test coverage tools to determine which parts of the code are not being tested and need to be covered.

# See Also

For more information on writing tests in Kotlin, check out the following resources:

- [Kotlin Test documentation](https://kotlinlang.org/docs/testing.html)
- [JUnit documentation](https://junit.org/junit5/)
- [Effective Testing in Kotlin](https://proandroiddev.com/effective-testing-in-kotlin-dd8b4ecadebc)