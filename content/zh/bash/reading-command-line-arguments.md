---
title:                "Bash: 读取命令行参数"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么阅读 Bash 命令行参数
Bash 是一个强大的命令行解释器，它可以让你通过在命令行中输入命令来与计算机进行交互。阅读命令行参数可以帮助你更有效地使用 Bash，使你的工作更加轻松和高效。

## 如何阅读命令行参数
阅读命令行参数很简单。你只需要在命令行中输入 `bash` 命令，然后加上一个 `-c` 参数，后面跟着你想要运行的命令。例如，如果你想要运行 `ls` 命令，并查看当前目录下的文件列表，你可以输入以下命令：

```Bash
bash -c "ls"
```

这样就会将 `ls` 命令作为参数传递给 `bash` 命令，从而实现了阅读命令行参数的功能。你也可以添加更多的参数，比如 `echo` 命令，用来打印出 `bash` 命令执行后的结果。

```Bash
bash -c "ls; echo '命令执行完毕！'"
```

## 深入了解阅读命令行参数
在 Bash 中，命令行参数是通过变量 `$@` 来表示的，它包含了所有传递给 `bash` 命令的参数。如果只想要读取其中的一个参数，可以使用 `$1`、`$2` 等来获取第一个、第二个参数，以此类推。如果想要获取所有参数的个数，可以使用 `$#` 来查看。这些变量都是内置的，可以在任何地方使用。

另外，你也可以使用 `getopts` 命令来读取命令行参数。它可以帮助你更方便地读取参数，并且提供了更多的选项和功能。同时，还可以使用 `shift` 命令来移动参数的位置，便于在不同的位置读取参数。

## 参考链接
- [Bash 参数](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameters.html)
- [Bash 中的 getopts 命令](https://www.gnu.org/software/bash/manual/html_node/The-Set-Builtin.html)
- [Bash 中的 shift 命令](https://www.gnu.org/software/bash/manual/html_node/Bourne-Shell-Builtins.html)

# 参见
- [Bash 入门指南](https://www.newcastlerunners.org/binmao/bash/)
- [Bash 教程 - 网易云课堂](https://study.163.com/course/courseMain.htm?courseId=1210004&_trace_c_p_k2_=4475a4ed413344c08078f65f0d591c71)