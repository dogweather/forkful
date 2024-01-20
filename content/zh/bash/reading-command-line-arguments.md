---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么与为什么？
命令行参数是当你运行程序时给它的输入。程序员读取这些信息以获取关于程序运行方式的特定指示。

## 如何：
让我们看一些在Bash中读取命令行参数的代码例子。以下脚本就是如何获取参数的一个例子：

```bash
#!/bin/bash

# 存储第一个参数
first_arg=$1

# 存储第二个参数
second_arg=$2

# 输出参数
echo "第一个参数是: $first_arg"
echo "第二个参数是: $second_arg"
```

运行这个脚本的命令通常如下：

```bash
bash script.bash arg1 arg2
```

运行这个命令的输出如下：

```bash
第一个参数是: arg1
第二个参数是: arg2
```

## 深入研究
当我们在早年的Unix shell中接触命令行参数时，同时也出现了$#用于指挥参数的数目。 在Bash中，我们有很多方式来处理命令行参数，比如使用shift命令来向前移动参数。

其他类似功能的命令解析器，比如Python的argparse，提供了更多复杂的选项，如默认值和长/短参数。在Bash中，getopt是处理更复杂参数的强大工具，它允许我们定义所需的参数类型，这样我们就可以读取长参数和短参数。

## 参考资料
以下是关于命令行参数和Bash编程的更多资源：

1. [Bash 脚本教程](https://linux.cn/article-4271-1.html)
2. [处理 Bash 命令行参数](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_09_07.html)
3. [使用getopt解析bash脚本参数](https://www.baeldung.com/linux/getopt-command)