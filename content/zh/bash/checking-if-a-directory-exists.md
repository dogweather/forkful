---
title:                "检查目录是否存在"
html_title:           "Bash: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

首先，它是一种常见的编程需求，毕竟在编程中我们需要处理很多文件和目录。其次，通过检查目录是否存在，我们可以避免产生不必要的错误，并可以更有效地处理文件操作。

## 如何进行

使用Bash中的test命令来检查目录是否存在，语法为：```test -d <目录路径>```。具体示例如下：

```
test -d /home/user/Downloads
echo $?
```

如果目录存在，输出将为0；如果目录不存在，输出将为1。

## 深入了解

除了使用test命令，我们还可以使用[bracket扩展](https://www.gnu.org/software/bash/manual/html_node/The-Conditional-Expression-Building.html)，语法为：```[ -d <目录路径> ]```。具体示例如下：

```
[ -d /home/user/Downloads ]
echo $?
```

同样的，如果目录存在，输出将为0；如果目录不存在，输出将为1。

## 参考链接

- [Bash官方文档](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [test命令的用法](https://linux.die.net/man/1/test)
- [bracket扩展的用法](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [检查目录是否存在的更多方法](https://stackoverflow.com/questions/59838/check-if-a-directory-exists-in-a-shell-script)