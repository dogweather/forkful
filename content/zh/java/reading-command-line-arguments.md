---
title:    "Java: 命令行参数阅读"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

我们在日常生活中经常使用电脑，但是有时候需要运行一些特定的程序，这时候就需要通过命令行来指定程序的运行参数。本文将介绍如何在Java中读取命令行参数，并提供一些实用的例子。

## 如何

通过使用Java的命令行参数，我们可以轻松地向程序传递一些特定的信息，从而使程序执行不同的操作。下面是一个简单的示例，演示如何读取并打印出命令行参数：

```Java
public class CommandLineArgs {
    public static void main(String[] args) {
        for (String arg : args) {
            System.out.println(arg);
        }
    }
}
```
运行该程序时，我们可以通过在命令行中输入参数来看到不同的输出，例如：

```
$ java CommandLineArgs apples oranges bananas
apples
oranges
bananas
```

我们也可以根据不同的参数来执行不同的操作，例如：

```Java
public class CommandLineArgs {
    public static void main(String[] args) {
        String fruit = args[0];
        switch (fruit) {
            case "apples":
                System.out.println("You have chosen apples!");
                break;
            case "oranges":
                System.out.println("You have chosen oranges!");
                break;
            case "bananas":
                System.out.println("You have chosen bananas!");
                break;
            default:
                System.out.println("Sorry, we don't have that fruit.");
                break;
        }
    }
}
```

运行该程序时，我们可以根据输入的参数来看到不同的输出，例如：

```
$ java CommandLineArgs apples
You have chosen apples!

$ java CommandLineArgs peaches
Sorry, we don't have that fruit.
```

通过利用命令行参数，我们可以让程序更具有灵活性和交互性，满足不同用户的需求。

## 深入探讨

在Java中，我们可以通过使用`args`数组来访问命令行参数。该数组的长度可以通过`args.length`来获取，通过`args[index]`可以访问特定位置的参数，其中`index`从0开始。

除了使用命令行参数，我们也可以利用`Scanner`类来从用户输入中读取数据。这种方法可以让我们在程序运行过程中动态地获取用户输入，并根据输入来决定程序的行为。

## 参考链接

- [Java命令行参数教程](https://www.baeldung.com/java-command-line-arguments)
- [Java Scanner类教程](https://www.w3schools.com/java/java_user_input.asp)
- [Java命令行参数API文档](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)

## 查看更多

了解更多有关Java编程的知识，可以查看以下文章：

- [Java基础教程](https://www.runoob.com/java/java-tutorial.html)
- [Java入门指南](https://www.w3schools.com/java/default.asp)
- [计算机编程语言排行榜](https://www.tiobe.com/tiobe-index/)