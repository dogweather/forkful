---
title:                "Writing tests"
aliases:
- en/kotlin/writing-tests.md
date:                  2024-02-03T19:03:33.785130-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests in Kotlin involves crafting code snippets that automatically validate the functional correctness of your software modules, ensuring they work as expected. Programmers do it to catch bugs early, facilitate code refactoring, and provide documentation on how software components are intended to work.

## How to:

Kotlin supports test-driven development with various frameworks, the most popular being JUnit, Kotest, and MockK for mocking. Here's a simple example using JUnit:

```kotlin
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class CalculatorTest {

    @Test
    fun `adds two numbers`() {
        val calculator = Calculator()
        val result = calculator.add(2, 3)
        assertEquals(5, result)
    }
}

class Calculator {
    fun add(a: Int, b: Int): Int = a + b
}
```

**Sample Output**

```text
Test passed.
```

For a more sophisticated testing approach using Kotest, which offers a more idiomatic Kotlin test writing style, see the example below:

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe

class CalculatorSpec : StringSpec({
    "adding 2 and 3 should return 5" {
        val calculator = Calculator()
        calculator.add(2, 3) shouldBe 5
    }
})
```

Using MockK for testing with mocks:

```kotlin
import io.mockk.every
import io.mockk.mockk
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class ServiceTest {

    private val repository = mockk<Repository>()
    private val service = Service(repository)

    @Test
    fun `get data returns mocked data`() {
        every { repository.getData() } returns "Mocked Data"

        val result = service.getData()

        assertEquals("Mocked Data", result)
    }
}

class Service(private val repository: Repository) {
    fun getData(): String = repository.getData()
}

interface Repository {
    fun getData(): String
}
```

**Sample Output**

```text
Test passed.
```

These examples illustrate the basics of writing unit tests in Kotlin. As your application grows, consider exploring more advanced testing techniques and tools provided by each framework.
