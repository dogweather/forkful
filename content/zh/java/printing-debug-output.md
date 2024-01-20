---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么和为什么?

打印调试输出是程序员在代码中决定显示其运行时信息的方法。程序员这样做是为了检查程序的状态，找出并解决问题。

## 怎么做:

在Java中，`System.out.println()`是用来打印调试输出的常用方法。让我们通过一个简单的例子来了解它:

```Java
public class DebugExample {
    public static void main(String[] args) {
        int a = 5;
        int b = 10;
        System.out.println("Debug: a is " + a + " and b is " + b);
        
        int sum = a + b;
        
        System.out.println("Sum of a and b is: " + sum);
    }
}
```
该程序的输出:

```Java
Debug: a is 5 and b is 10
Sum of a and b is: 15
```
## 深度解析

1. 历史背景: 打印调试输出的方法早在计算机程序出现之初就已经存在，并且仍然被广泛应用。虽然现代的调试工具强大且多样，打印调试输出由于其简单和方便仍然非常受欢迎。
2. 替代方案: 打印调试信息只是调试方法之一。其他的包括使用专门的调试工具，如Eclipse、Intellij等IDE的调试器，它们提供了更多深度调试的功能，如断点、单步执行等。
3. 实现细节: 在Java中，`System.out.println()`使用的是标准输出流来打印信息。这个标准输出流通常被操作系统重定向到命令行窗口或者其他的地方。

## 另请参阅

1. [Oracle Java Documentation on System.out](https://docs.oracle.com/javase/9/docs/api/java/lang/System.html#out): 更深入了解System.out和它是如何工作的。
2. [Java Debugging with Eclipse Tutorial](https://www.vogella.com/tutorials/EclipseDebugging/article.html): 学习如何在Eclipse IDE中使用调试器。
3. [Effective Java Logging](https://www.loggly.com/ultimate-guide/java-logging-basics/): 让你更加深入了解Java日志系统的指南。