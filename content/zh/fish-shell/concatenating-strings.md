---
title:                "连接字符串"
html_title:           "Fish Shell: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么

简单来说，如果你想要在命令行中创建、编辑和管理字符串，那么串联字符串就是一个非常有用的功能。串联字符串允许你将几个单独的字符串组合在一起，以创建一个全新的字符串。

## 如何操作

使用Fish Shell的串联字符串功能非常简单。你只需要使用`string combine`命令，然后在每个字符串之间使用空格分隔开即可。下面是一个例子：

```
Fish Shell Code Block:
string combine "Hello," "world!"
```

运行以上代码，你将会得到一个新的字符串"Hello, world!"。这个实例展示了如何将两个单独的字符串进行串联，并将它们合并为一个新的字符串。

## 深入了解

在Fish Shell中，串联字符串的功能不仅限于合并两个字符串。你还可以使用变量、命令替换和算术运算符来创建更加复杂的字符串。下面是一些示例：

```
Fish Shell Code Block:
set name "John"

# 使用变量
string combine "Hello, " "$name" "!"

# 使用命令替换
string combine "The current directory is: " (pwd)

# 使用算术运算符
string combine "5 + 5 = " (math 5 + 5)
```

在第一个例子中，我们使用了一个变量`name`来动态地创建一个问候字符串。在第二个例子中，我们使用了`pwd`命令替换来获取当前目录，并将它串联到一个提示消息中。最后，我们使用了算术运算符来计算5+5的结果，并将其作为一个字符串添加到另一个字符串的末尾。

## 参考链接

- [Fish Shell 官方文档](https://fishshell.com/docs/current/index.html)
- [Fish Shell - Concatenate Strings](https://fishshell.com/docs/current/cmds/string-combine.html)
- [GitHub- Fish Shell中文文档](https://github.com/fish-shell/fish-shell/blob/master/share/doc/Readme.zh_CN.md)

谢谢阅读本文，在你的下一次Fish Shell中，记得试试串联字符串的功能吧！

## 参见

- [Fish Shell的变量和命令替换](https://fishshell.com/docs/current/tutorial.html#variables-and-command-substitution)
- [Fish Shell的算术运算符](https://fishshell.com/docs/current/tutorial.html#arithmetic-operators)