---
title:                "Bash: 提取子字符串"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么会使用Bash提取子字符串

在Bash编程中，提取子字符串是一种常见的任务。它可以帮助程序员轻松地从一个字符串中获取所需的信息，例如某段文本或特定的字符。这可以提高编程效率，并简化一些复杂的操作。下面我们将介绍如何使用Bash提取子字符串。

## 如何提取子字符串

在Bash中，提取子字符串的最基本的命令是`${string:position:length}`。其中，`string`表示待提取的字符串，`position`表示提取的位置，`length`表示提取的长度。让我们来看一个简单的例子，假设我们有一个字符串`Hello World`，我们想要提取`World`这个词，可以使用以下代码：

```
Bash #!/bin/bash

string="Hello World"
substring=${string:6:4}

echo $substring
```

运行以上代码，将会输出`World`。在这个例子中，我们使用`6`作为`position`，因为`W`的位置是第6个字符。而`4`作为`length`，因为`World`有4个字符。

除了指定具体的`position`和`length`外，我们还可以使用以下更多的方法来提取子字符串：

- `${string:position}` : 从`position`开始提取字符串直到结尾
- `${string:position:-length}` : 从`position`开始提取字符串直到倒数第`length`个字符
- `${string: -position}` : 从开头开始提取字符串直到倒数第`position`个字符

值得注意的是，在Bash中，`position`和`length`都是从0开始计数的。因此，如果我们想要从第一个字符开始提取字符串，我们需要将`position`设置为`0`。

## 深入了解提取子字符串

除了以上基本的方法，Bash还提供了更加灵活的功能来提取子字符串，包括通配符（wildcards）、正则表达式（regular expressions）和替换（replacement）等。这些功能可以帮助我们更加精确地定位想要提取的内容。

另外，我们也可以使用循环来提取多个子字符串，或者结合其他命令（例如`grep`和`sed`）来进行更加复杂的操作。总而言之，提取子字符串的方法有很多，我们需要根据具体的情况选择最合适的方法。

## 参考文章

- [Bash String Manipulations](https://www.thegeekstuff.com/2010/07/bash-string-manipulation)
- [BashGuide/Substition](http://mywiki.wooledge.org/BashGuide/Substitutions#String_manipulation)
- [Advanced Bash-Scripting Guide - Chapter 10. Manipulating Variables](http://www.tldp.org/LDP/abs/html/string-manipulation.html)

## 参见

- [Bash文本处理教程](https://www.codecademy.com/learn/learn-the-command-line/modules/bash-scripting)
- [Bash快速入门教程](https://ryanstutorials.net/bash-scripting-tutorial/)