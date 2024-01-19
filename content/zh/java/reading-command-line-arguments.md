---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

---

## 什么和为什么? (What & Why?)

命令行参数是传递给程序的输入参数。程序员使用它是因为它使软件的运行更加灵活。

---

## 如何操作 (How to)

我们将介绍如何在Java中使用命令行参数。看这个简单的例子：

```Java
public class Main {

    public static void main(String[] args) {

        for (String arg : args) {
            System.out.println(arg);
        }
    }
}
```

如果你运行这段代码，并在调用时加上参数（如“Hello World”），输出就会是：

```
Hello
World
```

---

## 深入研究 (Deep Dive)

在早期的编程语言中，使用命令行参数来配置程序是常见的。Java作为一门现代化的语言，也加入了这个功能。

对于替代方案，Java还可以从输入流读取数据，通过网络接收数据，或者从配置文件加载参数。

在Java中，命令行参数被定义为`main`方法的参数。它们被存储为字符串数组，这是因为它们是从命令行接收的文本。

---

## 另请参阅 (See Also)

- [Java命令行参数（Oracle 官方文档）](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Java命令行参数（教程）](https://www.tutorialspoint.com/java/java_command_line_arguments.htm)