---
title:                "阅读命令行参数"
html_title:           "Java: 阅读命令行参数"
simple_title:         "阅读命令行参数"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么是读取命令行参数？为什么程序员要这么做？

读取命令行参数是指在编写Java程序时，程序员可以让程序在运行时接受命令行输入的指令或参数。这样可以使程序更加灵活多变，并根据不同的输入执行不同的操作。

## 如何读取命令行参数？

我们可以使用Java内置的命令行参数相关类来读取命令行传入的参数。下面是一个简单的示例代码：

```Java
public class CommandLineArgumentsExample {

    public static void main(String[] args) {
        // 输出传入的参数
        System.out.println("命令行参数：");
        for (String arg : args) {
            System.out.println(arg);
        }
    }
}
```

假设我们将以上代码保存为`CommandLineArgumentsExample.java`并在命令行中编译运行，则可以通过在命令行中输入参数来进行测试，例如：

```
java CommandLineArgumentsExample param1 param2
```

程序将会输出如下结果：

```
命令行参数：
param1
param2
```

需要注意的是，在使用命令行参数时，需要在程序运行命令中指定参数的数量和顺序，否则程序可能无法正常执行。

## 深入探讨

读取命令行参数这一技术其实早在Java发展初期就已经存在，并且在许多其它编程语言中也有类似的实现。除了使用Java内置的命令行参数相关类，我们还可以使用一些第三方库来处理命令行参数，例如Apache Commons CLI。另外，对于一些复杂的命令行参数处理需求，我们还可以使用命令行参数解析器，它可以帮助我们更加灵活地解析和处理命令行参数。

## 参考资料

- [Java命令行参数文档](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Apache Commons CLI网站](https://commons.apache.org/proper/commons-cli/)
- [Command Line Parameter Parser网站](https://github.com/jopt-simple/jopt-simple)