---
title:                "Bash: 找到字符串的长度"
simple_title:         "找到字符串的长度"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么：探索字符串长度的重要性
在Bash编程中，字符串是一种非常常见的数据类型。因此，了解如何找到字符串的长度是十分重要的。字符串的长度不仅可以帮助您更有效地处理数据，还可以提高程序的性能和可读性。在本文中，我将向大家介绍如何使用Bash编程语言来找到字符串的长度，并提供一些深入的知识。

## 如何找到字符串的长度
要想找到字符串的长度，我们需要使用Bash内置的 `expr` 命令。这个命令可以计算字符串的长度，并将结果输出到终端。

```Bash
sentence="这是一个测试字符串" # 定义一个字符串变量
length=`expr length "$sentence"` # 使用 `expr` 来计算字符串的长度，并将结果赋值给变量length
echo $length # 输出字符串的长度
```

上述代码的输出结果为：`10`，因为这个字符串包含10个字符。

## 深入探究
在Bash中，字符串的长度被定义为字符串中所有字符的总数，包括空格和标点符号。但是需要注意的是，如果使用 `expr` 命令计算中文字符串的长度时，结果可能会不准确。这是因为Bash默认按照字符编码来计算字符串的长度，中文字符的编码长度比英文字符要长，所以计算出来的长度与实际的长度可能会不符。如果要准确的计算中文字符串的长度，可以使用 `wc -m` 命令，它可以统计字符的实际数目。

另外，如果要找到字符串除去空格后的长度，可以使用 `tr -d " "` 命令来删除字符串中的所有空格，然后再用 `expr` 命令来计算长度。

## 参考链接
- [Bash的字符串操作命令](https://www.runoob.com/w3cnote/bash-strings-operation.html)
- [Linux命令大全](http://man.linuxde.net)

# 参考链接