---
title:                "阅读命令行参数"
html_title:           "Bash: 阅读命令行参数"
simple_title:         "阅读命令行参数"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

命令行参数是在Bash编程中经常用到的一种技巧，它可以使我们的脚本更加灵活和可定制。通过阅读本文，你将学会如何在Bash中读取命令行参数，并能够更好地理解其作用。

## 怎么做

在Bash中，我们可以使用`$1`，`$2`等特殊变量来读取命令行参数。例如，假设我们有一个名为`greeting.sh`的脚本，其内容为：

```Bash
#!/bin/bash
echo "Hello $1! Welcome to the Bash world!"
```

在运行该脚本时，我们可以在命令行中添加一个参数，比如`World`，来代替`$1`的位置，从而实现对该参数的读取和使用。示例输出如下：

```Bash
$ bash greeting.sh World
Hello World! Welcome to the Bash world!
```

在这个例子中，我们将`World`作为参数传递给了`$1`，因此脚本输出的内容中`$1`的部分被替换成了`World`。同理，如果我们在命令行中传递多个参数，就可以使用`$2`、`$3`等变量来读取后续的参数。

## 深入了解

除了特殊变量外，Bash还提供了其他几种方式来读取命令行参数：

1. 使用`shift`命令：这个命令可以将所有命令行参数向左移动一位，从而使得`$2`变成`$1`，`$3`变成`$2`，依此类推。这在处理多个参数时非常有用。
2. 使用`getopts`命令：这个命令可以帮助我们解析命令行参数并将其存储在变量中，使得我们可以更方便地对参数进行操作。
3. 使用`$@`、`$*`变量：这两个变量可以帮助我们一次性读取所有命令行参数，并将它们作为一个整体使用。

## 看看这些链接

- [Bash官方文档](https://www.gnu.org/software/bash/)
- [Bash相关教程](https://www.shellscript.sh/index.html)
- [通过命令行参数自动更新软件包](https://blog.packagecloud.io/eng/2016/03/14/building-debian-packages-with-bash/)