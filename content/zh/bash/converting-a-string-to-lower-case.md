---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么和为何？

字符串转换为小写是将字符串中的所有大写字母改为小写字母的过程。程序员这样做的主要原因是为了数据的一致性和简化字符串的比较。

## 如何操作：

Bash提供了一种简单的方法来实现这个操作。你可以使用tr命令执行此操作，下面是具体操作和样例输出。

```Bash
# 声明一个包含大写字母的字符串
str="Hello World!"

# 使用tr命令将字符串转换为小写
lowercase_str=$(echo "$str" | tr '[:upper:]' '[:lower:]')

# 打印转换后的字符串
echo $lowercase_str
```

上述代码的输出将会是：

```Bash
hello world!
```

## 深化挖掘：

转换字符串到小写在Unix和Linux操作系统的早期版本中就已经存在，而`tr`命令也提供了一种快速有效的方式来实现这个操作。虽然还有其他几种方式（比如使用`awk`命仔、`sed`命令等）同样可以实现转换，但`tr`命令由于其简洁性和效率，仍然是首选的方式。`tr`命令实际上执行了一次全字符的遍历和替换，使得操作效率非常高。

## 另见：

- Bash手册的`tr`命令部分：https://www.gnu.org/software/bash/manual/html_node/The-Sh-Shell.html
- 转换字符串大小写的其他方法：https://stackoverflow.com/questions/2264428/how-to-convert-a-string-to-lower-case-in-bash