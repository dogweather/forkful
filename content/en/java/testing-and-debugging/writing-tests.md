---
date: 2024-02-03 19:03:32.538932-07:00
description: "How to: Java developers primarily use two testing frameworks: JUnit\
  \ and TestNG. Here, we'll focus on JUnit, the more popular choice for writing tests\
  \ due\u2026"
lastmod: '2024-03-13T22:44:59.975583-06:00'
model: gpt-4-0125-preview
summary: Java developers primarily use two testing frameworks.
title: Writing tests
weight: 36
---

## How to:
Java developers primarily use two testing frameworks: JUnit and TestNG. Here, we'll focus on JUnit, the more popular choice for writing tests due to its simplicity and widespread adoption.

### JUnit Basics
To use JUnit in your Maven project, add the following dependency to your `pom.xml`:

```xml
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <version>5.9.0</version>
    <scope>test</scope>
</dependency>
```

A basic test in JUnit looks like this:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTest {
    
    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 should equal 5");
    }
}
```

Executing this test will either pass, indicating the `add` method works as expected, or fail, showing an error message.

### Mocking with Mockito
In real-world scenarios, objects often depend on other objects. Mockito is a popular mocking framework that helps in creating mock objects for the purpose of testing.

Add Mockito to your Maven project:

```xml
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <version>4.5.1</version>
    <scope>test</scope>
</dependency>
```

A simple use case with Mockito:

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class UserServiceTest {

    @Test
    public void testGetUsername() {
        // Create a mock UserRepository
        UserRepository mockRepository = mock(UserRepository.class);

        // Define behavior for mock object
        when(mockRepository.getUsername(1)).thenReturn("john_doe");

        UserService userService = new UserService(mockRepository);
        
        assertEquals("john_doe", userService.getUsername(1), "User ID 1 should be john_doe");
    }
}
```

This mock allows us to test `UserService` without needing an actual `UserRepository`, focusing the test on the logic within `UserService` itself.
