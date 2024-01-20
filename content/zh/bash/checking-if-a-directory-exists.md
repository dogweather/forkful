---
title:                "检查目录是否存在"
html_title:           "Bash: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Bash编程：如何检查一个目录是否存在

## 什么和为什么？

检查目录是否存在是一种用Bash脚本来识别系统里指定文件夹是否存在的方法。程序员经常这样做，以防止由于试图访问、修改或删除一个不存在的目录而引发的错误。

## 如何做：

检查一个目录是否存在的最基本方法，在Bash中是使用`-d`运算符。假设我们想查找的目录是`/tmp/mydir`。

```Bash
if [ -d "/tmp/mydir" ] 
then
    echo "目录存在"
else
    echo "目录不存在"
fi
```

如果`/tmp/mydir`存在，你会在shell输出中看到"目录存在"。否则，你会看到"目录不存在"。

## 深入了解

在UNIX和Linux出现早期，几乎所有的文件和目录操作都是由明确的shell命令完成的。但随着时间的推移，这些命令逐渐被各种测试运算符取代，这些运算符为Bash（和其他shell）提供了更大的灵活性和控制力。

对于检查目录是否存在，`-d`不是唯一的方式。你也可以使用`-e`运算符，它检查任何类型的文件（包括目录）是否存在。例如：

```Bash
if [ -e "/tmp/mydir" ] 
then
    echo "文件或目录存在"
else
    echo "文件或目录不存在"
fi
```

然而，在大多数情况下，我们确实想知道的是一个特定的'路径'是否存在作为一个目录，所以我们更常使用`-d`运算符。

## 参考资料：

1. [GNU Bash Manual - Conditional Expressions](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
3. [Testing Files in Bash](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_02.html)