---
title:                "Java: 读取命令行参数"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么要读取命令行参数？

当你开始学习Java编程时，你可能会想知道为什么要读取命令行参数。读取命令行参数可以让你的程序更加灵活，能够根据不同的输入做出不同的反应。这将有助于提高你的程序的可用性和用户体验。

# 如何读取命令行参数？

要读取命令行参数，你需要使用Java中的`args`变量。这个变量是一个字符串数组，包含了程序运行时传入的所有命令行参数。以下是一个简单的示例：

```Java
public class CommandLineArgs {
    public static void main(String[] args) {
        System.out.println("你输入的参数是：" + args[0]);
    }
}
```

如果你输入`java CommandLineArgs hello`，程序会输出`你输入的参数是：hello`。你可以根据需要读取不同位置的参数，比如`args[1]`、`args[2]`等等。

# 深入学习命令行参数的使用

除了基本的读取命令行参数外，你还可以使用一些Java中的库来简化读取参数的过程。比如，你可以使用Apache Commons CLI来处理不同类型的命令行参数，比如布尔值、数字等等。另外，你还可以使用JCommander来创建一个命令行工具，它能够自动生成帮助信息，并且支持嵌套命令等高级功能。

另外，你还可以通过使用命令行的帮助文档来深入了解如何更好地读取和处理命令行参数。一些常用的命令行帮助文档包括GNU Getopt和POSIX getopt。

# 参考资料

- Apache Commons CLI: http://commons.apache.org/proper/commons-cli/
- JCommander: http://jcommander.org/
- GNU Getopt: https://www.gnu.org/software/libc/manual/html_node/Getopt.html
- POSIX getopt: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/getopt.html

# 参见

- [Java命令行参数入门教程](https://www.jianshu.com/p/fd29791e3599)
- [Java命令行参数进阶教程](https://www.jianshu.com/p/bb11f8939b27)
- [使用Apache Commons CLI读取命令行参数](https://www.jianshu.com/p/820908615985)
- [使用JCommander创建命令行工具](https://www.jianshu.com/p/d1abfb96c3de)