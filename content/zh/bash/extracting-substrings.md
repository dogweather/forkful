---
title:    "Bash: 提取子字符串"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么：

提取子字符串是编程中常见的操作，它可以让我们从一个字符串中提取出我们需要的信息。例如，假设我们有一个包含很多商品信息的数据库，我们想要从中提取出每个商品的价格，这时候就可以使用提取子字符串的方法。

## 如何操作：

在Bash中，我们可以使用`cut`命令来提取子字符串。以下是一个简单的示例，假设我们有一个字符串"Hello World"，我们想要提取出其中的"World"，我们可以使用以下代码：

```Bash
echo "Hello World" | cut -d' ' -f 2
```

这段代码的意思是，使用空格作为分隔符（`-d' '`），提取出第2个字段（`-f 2`），即"World"。

## 深入了解：

除了使用`cut`命令，我们还可以使用Bash中的字符串截取技巧来提取子字符串。以下是一些常用的字符串截取方法：

- `${string:position}`：从字符串的指定位置开始提取子字符串。例如，`${string:5}`会提取出从第5个字符开始的子字符串。
- `${string:position:length}`：从指定位置开始提取指定长度的子字符串。例如，`${string:5:3}`会提取出从第5个字符开始长度为3的子字符串。
- `${string#substring}`：从字符串开头开始寻找指定子字符串并删除，然后返回剩余的字符串。例如，`${string#H}`会删除字符串开头的"H"，然后返回剩余的字符串"ello World"。
- `${string%substring}`：从字符串结尾开始寻找指定子字符串并删除，然后返回剩余的字符串。例如，`${string%ld}`会删除字符串结尾的"ld"，然后返回剩余的字符串"Hello Wor"。

无论是使用`cut`命令还是字符串截取方法，都能够轻松地提取出我们想要的子字符串。

## 查看也可以：

- [Linux Command Line: Master the Linux Command Line from Scratch](https://www.udemy.com/course/linux-command-line-from-scratch/?utm_source=adwords&utm_medium=udemyads&utm_campaign=Linux&utm_term=_._ag_90619066970_._ad_437497333164_._de_c_._dm__._pl__._ti_kwd-303100969360_._li_9065282_._pd__._&utm_term=_._pd__._kw_linux%20command%20line%20tutorial_._)
- [Bash Substring Operations](https://www.tldp.org/LDP/abs/html/string-manipulation.html#SUBSTRREF)