---
title:                "Java: 打印调试输出"
programming_language: "Java"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/printing-debug-output.md"
---

{{< edit_this_page >}}

本文将详细讲解Java程序员为何需要打印调试输出，如何进行打印调试输出，以及深入了解如何打印调试输出。

## 为什么

打印调试输出在开发过程中扮演着重要的角色。它可以帮助开发人员跟踪代码，排除错误，并提供程序的正确运行信息。

## 如何进行打印调试输出

首先，在需要调试的地方添加```System.out.println()```语句，它可以将想要打印的信息输出到控制台。例如：

```Java
String name = "Maggie";
System.out.println("姓名：" + name);
```

上述代码将会在控制台打印出“姓名：Maggie”。这样就可以很容易地查看程序运行过程中的变量值，从而帮助我们发现潜在的问题。

## 深入了解打印调试输出

除了简单地打印变量值之外，打印调试输出还可以帮助我们更好地理解代码的执行流程。例如，使用```System.err.println()```可以将信息输出到标准错误流，这样可以让我们在调试时更容易区分不同类型的输出信息。

此外，我们还可以使用断言（assert）来进行调试。断言类似于条件语句，当表达式为假时，会抛出AssertionError异常，并输出我们提供的错误信息。例如：

```Java
int age = 20;
assert age == 18 : "年龄不符合要求";
```

如果age的值不为18，程序将会输出“年龄不符合要求”。

## 参考链接

- [Java调试技巧：如何打印输出变量](https://www.runoob.com/java/java-print-var.html)
- [Java 断言（Assertion）](https://www.runoob.com/java/java-assertions.html)