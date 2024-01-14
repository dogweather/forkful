---
title:    "Fish Shell: 写入标准错误"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## 为什么要写到标准错误流

写入标准错误流是一种在命令行界面下记录错误信息的方式。当程序运行时出现错误，通常会将错误信息输出到标准错误流。这样可以方便程序员查看错误信息以及调试程序。通过学习如何写入标准错误流，你可以更有效地调试你的程序。

## 如何写入标准错误流

Fish Shell提供了一个有用的内置命令，用于将错误信息写入标准错误流。下面是一个简单的示例代码：

```
fish_err "这是一个错误信息"
```

运行上面的代码，你会在终端上看到类似以下的输出：

```
这是一个错误信息
```

如果你想要输出更详细的错误信息，你可以在命令行中使用管道和重定向符号来捕获标准错误流的输出，如下所示：

```
ls -l file_name 2>&1 | grep "No such file or directory"
```

这会将错误信息输出到标准输出流，并使用grep命令过滤出想要的内容。

## 深入了解标准错误流

标准错误流是一种非常有用的调试工具。通过将错误信息输出到标准错误流，你可以轻松地定位程序中的错误，并更快地修复它们。除了上面提到的使用方法，你还可以使用Fish Shell的其他内置命令和功能来更高效地处理标准错误流。

## 参考资料

- [Fish Shell官方文档](https://fishshell.com/docs/current/)
- [Fish Shell错误处理教程](https://fishshell.com/docs/current/tutorial.html#tut_errors)
- [标准输入输出流详解](https://www.ruanyifeng.com/blog/2019/09/bash-redirection.html)