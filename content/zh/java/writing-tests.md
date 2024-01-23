---
title:                "编写测试代码"
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
写测试就是编写代码验证其他代码是否按预期工作。程序员这么做是为了确保代码质量，提前发现bug，以及促进安全的代码重构。

## How to: (如何做)
```java
import org.junit.jupiter.api.*;

public class CalculatorTest {

    private Calculator calculator = new Calculator();

    @BeforeEach
    void setUp() {
        calculator.clear();
    }

    @Test
    void testAdd() {
        Assertions.assertEquals(5, calculator.add(2, 3));
    }

    @AfterEach
    void tearDown() {
        calculator.clear();
    }
}

class Calculator {
    private int result;

    void clear() {
        result = 0;
    }

    int add(int a, int b) {
        result = a + b;
        return result;
    }
}
```
输出：
```
测试成功 (Test successful)
```

## Deep Dive (深入了解)
单元测试可追溯至1950年代。JUnit框架的推出标志着Java单元测试的新时代。除了JUnit，还有TestNG、Spock等替代测试框架。详细实现方面，测试框架使用反射调用测试方法，通常配合mocking工具来模拟外部依赖。

## See Also (另请参阅)
- JUnit 5用户指南：https://junit.org/junit5/docs/current/user-guide/
- TestNG官方文档：https://testng.org/doc/
- Mockito：模拟框架，用于创建和配置测试中的mock对象。
