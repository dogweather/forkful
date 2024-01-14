---
title:    "Bash: 将字符串大写"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

命令行脚本是日常计算机任务中必不可少的工具。无论是对文件进行操作，还是管理系统设置，都可以通过编写命令行脚本来实现。而在编写命令行脚本时，经常会用到字符串的大写功能，使得字符串的呈现更加直观和易读。因此，学习如何在Bash中实现字符串的大写功能是非常有用的。

## 如何实现

要想在Bash中实现字符串的大写，可以使用内置的tr命令。该命令可以将给定字符串中的小写字母转换为大写字母。下面是一个示例：

```Bash
echo "hello world" | tr [a-z] [A-Z]
```

该命令首先通过管道将字符串“hello world”传递给tr命令，然后将所有小写字母[a-z]替换为大写字母[A-Z]。运行这个命令后，会得到输出“HELLO WORLD”。

## 深入了解

除了使用tr命令，Bash中还有其他实现字符串大写的方法。比如，使用awk命令或者sed命令也可以实现相同的效果。另外，还可以使用Bash内置的${parameter^^pattern}语法来将字符串中符合模式匹配的部分转换为大写。此外，可以在man手册中查询更多有关字符串转换的方法。

## 参考链接

- [Bash手册](https://www.gnu.org/software/bash/manual/bash.html#Manipulating-Strings)
- [tr命令文档](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Awk命令文档](https://www.gnu.org/software/gawk/manual/html_node/Format-Control-Sequences.html)
- [Sed命令文档](https://www.gnu.org/software/sed/manual/html_node/sed-commands-list.html)

## 参考

- `Shell编程基础`： 钱文超