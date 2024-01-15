---
title:                "从命令行读取参数"
html_title:           "Java: 从命令行读取参数"
simple_title:         "从命令行读取参数"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么要读取命令行参数

读取命令行参数是很常见的一种程序设计方法，在许多Java项目中都可能会用到。通过读取命令行参数，我们可以在程序运行时输入一些特定的参数来控制程序的行为，使得程序更加灵活和可配置。

## 如何操作

在Java中，我们可以使用`args`数组来获取命令行参数，它会自动获取程序运行时后面输入的所有参数。下面是一个简单的例子：

```Java
public static void main(String[] args) {
    for (String arg : args) {
        System.out.println(arg);
    }
}
```

如果我们在命令行中输入命令`java MyClass hello world`，那么程序就会将`hello`和`world`分别打印出来。当然，我们也可以通过判断`args`数组的长度来确定是否有输入参数，以及对输入参数进行相应的处理。

## 深入了解

除了获取用户输入的参数之外，我们还可以通过`System.getProperty()`方法来读取系统属性。比如，运行`java -Dmy.property=test MyClass`，然后在程序中调用`System.getProperty("my.property")`就可以获取到`test`这个值。

另外，命令行参数也可以用来传递程序之间的通信信息，比如在启动另一个Java程序时传入一些参数来控制其行为。

## 参考链接

- [Java Command Line Arguments](https://www.tutorialspoint.com/java/java_command_line_arguments.html)
- [Java System Properties](https://docs.oracle.com/javase/tutorial/essential/environment/sysprop.html)
- [Java Communication Between Processes](https://docs.oracle.com/javase/tutorial/rmi/functinterface.html)