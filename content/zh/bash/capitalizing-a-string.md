---
title:                "将字符大写"
html_title:           "Bash: 将字符大写"
simple_title:         "将字符大写"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么是大写字符串？为什么程序员要这么做？

大写字符串指的是把字符串中的所有字母都改成大写形式。程序员经常这么做是因为大写字符串在编程中有很多方便之处。比如，大写字符串可以使得两个字符串的比较更加简单、快速；也可以使得字符串的输出更加规范、易读。

## 如何操作：

在Bash中，我们可以使用内置的tr命令来实现大写字符串的功能。具体的代码如下：
```
str="hello world"
str_caps=$(echo "$str" | tr '[:lower:]' '[:upper:]')
echo "$str_caps"
```
输出为：
```
HELLO WORLD
```

## 深入了解：

大写字符串的概念起源于早期计算机的ASCII编码中，其中只有大写字母，没有小写字母。随着编程语言的发展，大写字符串也被广泛应用于不同的编程语言中，比如C、Java等。

除了使用tr命令，程序员还可以通过自定义函数的方式来实现大写字符串。另外，也可以使用其他工具来实现类似的功能，比如sed命令、awk命令等。

在实现大写字符串的过程中，需要注意的是不同的编程语言对于大小写的敏感程度可能不同，因此在使用时需要留意。

## 参考资料：

- Bash官方文档：https://www.gnu.org/software/bash/manual/bash.html
- tr命令使用文档：https://www.tutorialspoint.com/unix_commands/tr.htm
- Bash中的字符串操作：https://www.lifewire.com/string-manipulation-in-unix-commands-4012823