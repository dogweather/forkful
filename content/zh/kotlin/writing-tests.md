---
title:                "编写测试代码"
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
测试是确保代码正确运行的方式。它可以及时发现问题，省时，保障质量。

## 如何：
```Kotlin
// 引入Kotlin测试库
import org.junit.Test
import kotlin.test.assertEquals

class ExampleUnitTest {
    @Test
    fun addition_isCorrect() {
        assertEquals(4, 2 + 2)
    }
}

// 运行测试，输出结果
Test passed
```

## 深入了解
编写测试的实践始于软件工程早期。JUnit是早期流行的Java测试框架，Kotlin兼容JUnit。除JUnit外，Kotlin还有MockK、Spek等测试库。编写测试，评估可选择基于断言的测试、行为驱动开发(BDD)或模拟对象。

## 参考资料
- JUnit 5 用户指南: [https://junit.org/junit5/docs/current/user-guide/](https://junit.org/junit5/docs/current/user-guide/)
- MockK 库: [https://mockk.io/](https://mockk.io/)
- Spek 框架: [https://www.spekframework.org/](https://www.spekframework.org/)