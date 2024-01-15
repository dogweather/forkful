---
title:                "将字符串大写化。"
html_title:           "Bash: 将字符串大写化。"
simple_title:         "将字符串大写化。"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在Bash编程中，我们经常需要处理字符串。有时，我们希望将字符串的第一个字符大写，这可能是为了让输出更美观，或者符合特定的命名规范。无论原因如何，通过本文，你将学习如何在Bash中快速、简单地将一个字符串的首字母大写。

## 如何

在Bash中，我们可以使用`${variable^}`的语法来将变量的第一个字符大写。下面是一个例子：

```Bash
name="john"
echo "${name^}"
```

运行这段代码，输出将会是`John`，首字母J变成了大写。同样，我们也可以使用`${variable^^}`来将整个字符串转换为大写。如果我们希望只将某个字符串变量的第一个字符大写，可以使用`${variable:0:1}`来截取第一个字符，然后与大写的`${variable^}`合并。下面是一个完整的例子：

```Bash
name="sarah"
capitalized="${name:0:1}${name^:1}"
echo "${capitalized}"
```

运行这段代码，输出将会是`Sarah`，首字母S也变成了大写。

## 深入探究

除了上面提到的这种方法，我们也可以使用Bash内置的命令`tr`来实现将字符串的首字母大写的功能。`tr`命令可以用来替换、删除或转换字符串中的字符。下面是一个使用`tr`命令来实现字符串首字母大写的例子：

```Bash
name="tom"
capitalized=$(echo "${name}" | tr '[:lower:]' '[:upper:]')
echo "${capitalized}"
```

运行这段代码，输出将会是`Tom`，同样实现了字符串首字母大写的效果。关于`tr`命令的更多用法和参数可以通过使用`man tr`命令来查看帮助文档。

## 参考资料

- [Bash Reference Manual: Shell Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
- [Bash Guide for Beginners: Basic String Operations](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_09_01.html)
- [Bash Cookbook: Modifying Strings](https://www.oreilly.com/library/view/bash-cookbook/9780596526788/ch03s10.html)

## 参见

- [Bash中如何截取字符串？](https://www.google.com/search?q=bash+string+substring)
- [如何在Bash中实现字符串替换？](https://www.google.com/search?q=bash+string+replace)