---
title:                "编写测试"
aliases:
- zh/java/writing-tests.md
date:                  2024-02-03T19:31:01.754928-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写测试"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 Java 中编写测试的目的是为了验证你的代码在各种条件下的行为是否符合预期。程序员编写测试来防止 bug，确保在更改后功能仍然正确，并促进良好的软件设计原则。

## 如何：
Java 开发人员主要使用两个测试框架：JUnit 和 TestNG。这里，我们将关注 JUnit，由于其简单性和广泛的采用，它是编写测试的更受欢迎的选择。

### JUnit 基础

要在你的 Maven 项目中使用 JUnit，请将以下依赖项添加到你的 `pom.xml` 中：

```xml
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <version>5.9.0</version>
    <scope>test</scope>
</dependency>
```

在 JUnit 中，一个基本测试看起来像这样：

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CalculatorTest {
    
    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 应该等于 5");
    }
}
```

执行这个测试将会通过，表明 `add` 方法的工作如预期，或者失败，显示错误消息。

### 使用 Mockito 进行模拟

在现实世界的场景中，对象经常依赖于其他对象。Mockito 是一个流行的模拟框架，有助于为测试目的创建模拟对象。

将 Mockito 添加到你的 Maven 项目中：

```xml
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <version>4.5.1</version>
    <scope>test</scope>
</dependency>
```

使用 Mockito 的一个简单用例：

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class UserServiceTest {

    @Test
    public void testGetUsername() {
        // 创建一个模拟 UserRepository
        UserRepository mockRepository = mock(UserRepository.class);

        // 为模拟对象定义行为
        when(mockRepository.getUsername(1)).thenReturn("john_doe");

        UserService userService = new UserService(mockRepository);
        
        assertEquals("john_doe", userService.getUsername(1), "用户 ID 1 应该是 john_doe");
    }
}
```

这种模拟允许我们测试 `UserService` 而不需要一个真实的 `UserRepository`，将测试的焦点放在 `UserService` 本身的逻辑上。
