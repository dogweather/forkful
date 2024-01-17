---
title:                "将字符串转换为小写"
html_title:           "Bash: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么是字符串转换为小写？为什么程序员要这样做？

字符串转换为小写是将字符串中的所有字母变为小写的过程。程序员这样做的原因是为了方便比较字符串，因为大小写字母被认为是不同的字符。

## 如何进行字符串转换为小写？

你可以使用Bash的内置函数tolower来实现字符串转换为小写。以下是一个示例代码和输出结果：

```
string="HELLO"
result=`echo $string | tr '[A-Z]' '[a-z]'`
echo $result
```
输出结果为：hello

## 深入了解

历史背景：字符串转换为小写是受到了UNIX系统的启发，因为UNIX系统对大小写敏感，这意味着在比较文件名、命令等时，大小写是有区别的。

其他替代方案：除了使用内置函数tolower，你还可以使用sed、awk等工具来实现字符串转换为小写。

实现细节：内置函数tolower使用的是字符映射表来实现大小写转换。

## 参考资料

- [Bash内置函数tolower文档](https://www.gnu.org/software/bash/manual/html_node/String-Case-Modification.html)
- [字符串操作命令示例](https://www.geeksforgeeks.org/string-manipulation-in-shell-scripting/)