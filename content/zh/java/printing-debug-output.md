---
title:                "Java: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/printing-debug-output.md"
---

{{< edit_this_page >}}

为什么：为什么会需要打印调试输出？在开发过程中，打印调试输出可以帮助我们查看代码的执行过程中的各种变量的值，以便于我们更好地理解程序的运行情况和进行错误分析。

如何做到：我们可以使用Java语言提供的`System.out.println()`语句来输出调试信息。例如：

```Java
int num1 = 5;
int num2 = 7;
System.out.println("num1的值为：" + num1);
System.out.println("num2的值为：" + num2);
```

输出结果为：

```
num1的值为：5
num2的值为：7
```

这样，我们就可以清楚地看到num1和num2的值，从而判断程序的运行是否符合我们的预期。

深入了解：在实际开发中，我们可能会遇到复杂的调试需求，比如想要打印出对象的所有属性值，这时候可以利用Java中的`toString()`方法来实现。同时，我们也可以通过使用调试工具来更加方便地输出和追踪调试信息。

## 参考资料

- [Java: print debugging statement](https://stackoverflow.com/a/19600751)
- [Java Tutorials: Debugging](https://docs.oracle.com/javase/tutorial/java/nutsandbolts/andbolts/bugs.html)
- [JVM Debugging tips and techniques](https://www.javaworld.com/article/2077844/jvm-debugging-tips-and-techniques.html)

## 参见

- [Java语言入门教程](https://www.w3cschool.cn/java/)
- [Java编程思想](https://book.douban.com/subject/2130190/)
- [Java编程实践](https://book.douban.com/subject/30402848/)