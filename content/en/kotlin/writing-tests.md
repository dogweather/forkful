---
title:                "Writing tests"
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Writing tests means scripting code to check if other code works right. Programmers do it to catch bugs early, save time, and ensure the software does what it's supposed to do consistently.

## How to:
Kotlin uses JUnit for testing. Here's how to write and run a simple test:

```kotlin
import org.junit.Assert.assertEquals
import org.junit.Test

class CalculatorTest {
    
    @Test
    fun `adds two numbers`() {
        assertEquals(4, Calculator.add(2, 2))
    }
}

object Calculator {
    fun add(a: Int, b: Int) = a + b
}
```

Run it. If your output's like this, you're golden:

```
Test passed
```

## Deep Dive
JUnit, the go-to framework for testing in Kotlin, trails back to Java. Alternative test frameworks include Spek and Kotest, each having distinct syntaxes and features. Writing tests often involves understanding the SUT (System Under Test) structure, mocking dependencies with MockK or similar, and knowing the difference between unit, integration, and functional testing.

## See Also
- JUnit 5 User Guide: [junit.org/junit5/docs/current/user-guide/](https://junit.org/junit5/docs/current/user-guide/)
- MockK Library: [mockk.io](https://mockk.io)
- Spek Framework: [spekframework.org](https://spekframework.org)
- Kotest: [kotest.io](https://kotest.io)
