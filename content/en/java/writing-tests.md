---
title:                "Writing tests"
date:                  2024-01-19
simple_title:         "Writing tests"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests is crafting code that checks if other code functions correctly. Programmers do it to catch bugs early, ensure software works as expected, and maintain code quality over time.

## How to:

Let's write a simple test using JUnit, a popular testing framework in Java. We’ll test a method that adds two integers.

```java
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

public class CalculatorTest {

    @Test
    public void testAddition() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 should equal 5");
    }
}

class Calculator {
    public int add(int a, int b) {
        return a + b;
    }
}
```

If the method works, the test passes silently. If it fails, JUnit prints an error:

```
org.opentest4j.AssertionFailedError: 2 + 3 should equal 5 ==> expected: <5> but was: <4>
```

## Deep Dive

Testing wasn’t always a programmer’s priority—it gained traction with Agile development and practices like Test-Driven Development (TDD). Alternatives to JUnit include TestNG and Spock, each with its own perks. Implementing good tests is an art; it usually involves mocking dependencies, adhering to testing patterns, and continuously integrating tests into the build process.

## See Also

- JUnit 5 User Guide: [https://junit.org/junit5/docs/current/user-guide/](https://junit.org/junit5/docs/current/user-guide/)
- Article on Test-Driven Development: [https://www.agilealliance.org/glossary/tdd/](https://www.agilealliance.org/glossary/tdd/)
- Mocking frameworks: Mockito [https://site.mockito.org/](https://site.mockito.org/)
