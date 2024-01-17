---
title:                "提取子字符串。"
html_title:           "Bash: 提取子字符串。"
simple_title:         "提取子字符串。"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么是提取子字符串，为什么程序员要这么做？

提取子字符串是指从一个较长的字符串中截取出特定的短字符串。程序员经常会这么做是因为在处理大量字符串时，需要用到特定的子串。比如，一个程序需要从用户输入的字符串中找出特定的关键词，或者需要从一个文件路径中提取文件名，这些都需要用到提取子字符串的技巧。

## 如何提取子字符串？

通过使用Bash中的字符串提取命令，可以轻松地从一个字符串中提取出需要的子串。下面是一个示例代码，通过提取子字符串来获取文件名：

```Bash
# 定义一个包含文件路径的字符串
filepath="/Users/username/Documents/example.txt"
# 使用字符串提取命令从路径中提取出文件名
filename=${filepath##*/}
# 打印文件名
echo $filename
```

运行以上代码，输出结果为：example.txt。可以看到，通过使用字符串提取命令，我们可以方便地从一个复杂的字符串中提取出我们需要的子串。

## 深入了解

提取子字符串的概念不仅仅存在于Bash中，也是在其他编程语言中经常使用的技术。在过去，程序员可能会手动使用循环或者正则表达式来实现字符串提取，但是随着字符串提取命令的出现，这一过程变得更加简单高效。除了Bash字符串提取命令外，还有一些其他的工具和技巧可以实现字符串提取，如使用awk命令或者使用sed命令。

如果想要更深入地了解提取子字符串的原理和使用方法，可以查看Bash的官方文档或者参考其他相关资源。

## 查看更多

- [Bash文档](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameter-Expansion)
- [awk命令参考](https://www.gnu.org/software/gawk/manual/html_node/String-Functions.html)
- [sed命令参考](https://www.gnu.org/software/sed/manual/sed.html)