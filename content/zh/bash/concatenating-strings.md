---
title:                "拼接字符串"
html_title:           "Bash: 拼接字符串"
simple_title:         "拼接字符串"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

首先想想，你是如何创建一个完整的句子的？你可能会使用多个单词并将它们连在一起，对吧？那么，在编程中，当我们想要创建一个完整的文本时，我们也需要做同样的事情。这就是为什么我们需要学习如何连接字符串的原因。

## 如何

为了连接字符串，我们需要使用Bash编程语言中的特定命令：`echo`。它可以将你输入的文本打印输出到终端。让我们来看一个例子：

```Bash
echo "Hello" "world"
```

当我们运行这条命令时，它会打印出 `Hello world`，因为Bash会自动将两个字符串连接起来。你也可以使用变量来连接字符串，例如：

```Bash
first_name="John"
last_name="Doe"
echo $first_name $last_name
```
这样就会打印出 `John Doe`。

## 深入探讨

除了上面提到的方法外，我们还可以使用 `+=` 符号来连接字符串。让我们来看一个例子：

```Bash
greeting="Hello"
greeting+=" world"
echo $greeting
```
这将会打印出 `Hello world`。我们也可以使用单引号或双引号来连接字符串。使用单引号时， Bash会将其视为一个完整的字符串，而使用双引号时，Bash会将其中的变量内容进行替换后再连接。

## 参考连接

- [Bash 字符串的连接](https://www.runoob.com/w3cnote/shell-concat.html)
- [Shell 脚本中如何连接字符串](https://blog.csdn.net/tcl193/article/details/82202134)
- [Bash 文档](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)

## 参见

其他有用的Bash编程文章：[Bash Shell 教程](https://www.runoob.com/linux/linux-shell.html)