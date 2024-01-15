---
title:                "提取子串"
html_title:           "Bash: 提取子串"
simple_title:         "提取子串"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么要提取子字符串

在Bash编程中，提取子字符串是一种常用的技术。它可以帮助我们从一个字符串中获取有用的信息，比如提取文件名、删除多余的字符或将字符串拆分为更小的部分。无论你是在编写脚本还是进行数据处理，提取子字符串都是非常有用的技巧。

## 如何进行提取子字符串

提取子字符串的方法有很多种，我们将介绍其中最常用的几种方法并附上相应的代码示例和输出结果。首先，假设我们有一个字符串变量`str="Hello World!"`，我们将使用这个变量来进行实例演示。

### 使用`cut`命令提取固定位置的字符

使用`cut`命令可以提取字符串中固定位置的字符或字符范围。在下面的例子中，我们将使用`-c`选项来指定要提取的位置，并使用`$`符号来获取变量的值。

```Bash
str="Hello World!"
cut -c 1-5 <<< $str
```

输出结果为`Hello`，因为`1-5`表示提取第1到第5个字符。

### 使用`grep`命令提取匹配的字符

`grep`命令可以帮助我们从字符串中提取符合特定模式的字符。在下面的例子中，我们将使用正则表达式`[0-9]`来匹配字符串中的所有数字。

```Bash
str="Hello123World"
grep -o '[0-9]' <<< $str
```

输出结果为`123`，因为`-o`选项表示只显示匹配的部分。

### 使用变量替换提取特定子字符串

如果我们只想提取字符串中的特定部分，而不是固定位置或匹配的字符，可以使用变量替换来实现。在下面的例子中，我们将使用`#`和`%`来指定开始和结束的位置，并删除对应的子字符串。

```Bash
str="Hello World!"
echo ${str#Hello} # 打印" World!"
echo ${str%World!} # 打印"Hello "
```

输出结果分别为` World!`和`Hello `，因为`#`表示从开头删除`Hello`及之前的部分，`%`表示从结尾删除`World!`及之后的部分。

## 深入了解提取子字符串

除了上面介绍的方法外，还有许多其他技术可以用于提取子字符串。比如使用`awk`命令、使用`sed`命令等等。想要深入了解这些技术，可以阅读Bash官方文档或者在线资源。

## 看看这些链接

- [Bash官方文档](https://www.gnu.org/software/bash/manual/)
- [精通Bash编程：从Shell脚本到高级编程](https://book.douban.com/subject/35114363/)
- [Linux命令行与Shell脚本编程大全](https://book.douban.com/subject/25844639/)
- [Bash常用命令大全](https://book.douban.com/subject/3112799/)