---
title:                "Kotlin recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Why

As a developer, writing tests may seem like an extra step in the coding process and it can be tempting to skip them. However, writing tests is an important practice that can save time and prevent bugs from causing major issues in your code. In this blog post, we will delve into the importance of writing tests and how to do so effectively in Kotlin.

## How To

Writing tests in Kotlin is fairly straightforward and follows a similar structure to other programming languages. Let's take a look at an example of a simple function that adds two numbers and how we can write a test for it:

```kotlin
fun addNumbers(a: Int, b: Int): Int {
    return a + b
}
```

To test this function, we first need to create a test case using the `@Test` annotation. Within the test case, we can use the `assertEquals()` function to check if the output of our `addNumbers` function matches the expected result.

```kotlin
@Test
fun testAddNumbers() {
    val result = addNumbers(5, 10)
    assertEquals(15, result)
}
```

Now, when we run our test, we will get a passing result since the output of our `addNumbers` function matches the expected result. Writing tests like this not only ensures that our code is functioning correctly but also serves as a form of documentation for future reference.

## Deep Dive

Writing tests also allows us to catch bugs early on in the development process. By writing tests for each function, we can easily pinpoint any errors and fix them before they cause bigger issues in our code.

Additionally, tests can help with the overall structure and design of our code. By writing tests, we are forced to think about different scenarios and edge cases, resulting in cleaner and more robust code.

It's also worth noting that writing tests is not a one-time task. As our code evolves and new features are added, we should also update and add tests accordingly to ensure that everything continues to function as expected.

## See Also

- [Unit Testing in Kotlin](https://kotlinlang.org/docs/tutorials/https://kotlinlang.org/docs/tutorials/unittests.html)
- [The Art of Writing Testable Code](https://medium.com/@victorsavkin/testability-4230e4abc3c5)
- [Test-Driven Development in Kotlin](https://www.raywenderlich.com/5493-test-driven-development-in-kotlin)