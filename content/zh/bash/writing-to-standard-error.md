---
title:                "标准错误的写作"
html_title:           "Bash: 标准错误的写作"
simple_title:         "标准错误的写作"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

为什么我们会有必要在Bash中编写到标准错误？事实上，写入标准错误可以帮助我们更容易地调试代码，因为它可以打印出错误信息，帮助我们找出问题的根源。

## 如何做

我们可以使用`echo`命令将文本写入标准错误，语法如下：
```Bash
echo "错误信息" >&2
```
下面是一个简单的例子，输出错误信息"未找到文件"：
```Bash
echo "未找到文件" >&2
```
运行命令后，输出结果如下：
```
未找到文件
```
可以看到，错误信息被打印到了标准错误中。除了`echo`命令，我们还可以使用其他命令或者脚本来产生错误信息，比如在脚本中使用`exit`命令强制退出，并指定一个错误代码：
```Bash
exit 1
```
运行脚本后，会返回一个错误代码为1的错误信息。

## 深入了解

在Bash中，标准输出和标准错误是两个不同的输出流，它们分别用于打印普通的输出和错误信息。默认情况下，标准输出会显示在屏幕上，而标准错误则会显示在屏幕的红色文字中，以区分出错的信息。

除了使用`echo`命令和`exit`命令，我们也可以使用重定向符号`>`和`2>`来将文本写入标准错误。比如，将标准输出重定向到`/dev/null`，将标准错误重定向到标准输出：
```Bash
ls -l file1 file2 1>/dev/null 2>&1
```
这条命令会将`ls`命令的标准输出重定向到`/dev/null`，意为将输出信息丢弃，然后将标准错误重定向到标准输出，这样错误信息就会显示在屏幕上。

## 参考资料

- [Bash Reference Manual: Redirections](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
- [Standard Streams: What are the three default standard streams provided by the shell?](https://unix.stackexchange.com/questions/506994/what-are-the-three-default-standard-streams-provided-by-the-shell)