---
title:                "打印调试输出"
html_title:           "Java: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/printing-debug-output.md"
---

{{< edit_this_page >}}

# 为什么要打印调试输出(Debug Output)

作为一个Java程序员，你可能会在写代码时遇到各种各样的错误和bug。当你无法找出错误的原因时，打印调试输出(debug output)可以帮助你更快地定位问题并进行修复。它可以显示程序的运行过程中每一步的具体信息，让你更加了解程序的运行情况。

## 如何打印调试输出

要打印调试输出，我们可以使用Java语言中的System.out.println()方法。这个方法可以将要打印的信息作为参数传入，并在控制台上显示出来。下面是一个简单的示例：

```Java
public class DebugOutputExample {

    public static void main(String[] args) {
        int a = 10;
        int b = 5;
        // 打印a和b的值
        System.out.println("a的值为：" + a);
        System.out.println("b的值为：" + b);
        // 计算a和b的和，并打印结果
        int sum = a + b;
        System.out.println("a和b的和为：" + sum);
    }
}

// 输出结果：
// a的值为：10
// b的值为：5
// a和b的和为：15
```

通过打印a和b的值，我们可以确保变量的值是正确的。而通过打印a和b的和，我们可以验证我们的计算是否正确。这对于调试代码非常有用。

## 深入探讨调试输出

打印调试输出是Java程序员必备的技能之一。通过打印不同的信息，我们可以更好地理解程序的运行情况，从而快速定位和解决问题。除了使用System.out.println()方法，我们还可以使用其他方法如System.out.printf()来打印更复杂的信息。此外，我们还可以使用日志框架如log4j来管理显示调试输出，使调试更加高效。

# 参考链接

- [Java Debugging with Eclipse: Part 2](https://www.cs.ucsb.edu/~edu/courses/cs24n/JavaDebugEclipse/JavaDebuggingEclipse_Part2.pdf)
- [Debugging in Java](https://www.geeksforgeeks.org/debugging-in-java/)
- [Using Debugging and Profiling Tools](https://docs.oracle.com/javase/8/docs/technotes/guides/jvmti/jvmti.html#debugging)