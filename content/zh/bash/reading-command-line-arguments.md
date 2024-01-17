---
title:                "读取命令行参数"
html_title:           "Bash: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么是命令行参数
命令行参数是指通过命令行传递给程序的信息。程序员通常需要读取这些命令行参数来根据用户的需求进行不同的操作。

## 如何读取命令行参数
可以使用Bash内置的`$1`、`$2`等变量来获取传入的参数。比如`$1`表示第一个参数，`$2`表示第二个参数，以此类推。在下面的例子中，我们将打印出传入的第一个和第二个参数。
```Bash
echo "第一个参数为: $1"
echo "第二个参数为: $2"
```
**例子输入** `sh script.sh hello world`

**例子输出**
```
第一个参数为: hello
第二个参数为: world
```

## 深入了解
- 历史背景：在早期的操作系统中，命令行参数的重要性更加突出，因为它是与用户交互的唯一途径。随着图形用户界面的发展，命令行的使用不再那么广泛。
- 替代方法：除了读取命令行参数，程序员还可以使用环境变量、配置文件等方式来获取程序运行时的设置信息。
- 实现细节：在Bash中，`$1`、`$2`等变量其实是特殊的环境变量，存储着用户传入的参数值。
## 相关链接
- 了解更多Bash编程相关的知识：https://www.gnu.org/software/bash/
- 关于命令行参数的详细介绍：https://tldp.org/LDP/abs/html/abs-guide.html#COMMANDLINEARGS
- 在Shell脚本中使用环境变量：https://www.computerhope.com/unix/bash/texport.htm