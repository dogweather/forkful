---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么和为什么?
字符串子串提取是一种在Bash编程中取出字符串一部分的技术。这样做是因为我们广泛使用它来解析和操作数据。

## 如何操作:
在Bash中，你可以使用内建子串提取方法从字符串中提取一个字符串。这是几种方法：

1. 使用字符串位置和长度提取子串：
```Bash
my_string="Hello, World"
echo ${my_string:7:5}
```
输出：
```Bash
World
```
2. 使用字符串位置提取从该位置到末尾的所有字符：
```Bash
my_string="Hello, World"
echo ${my_string:7}
```
输出：
```Bash
World
```

3. 使用负数位置从末尾提取字符：
```Bash
my_string="Hello, World"
echo ${my_string: -5}
```
输出：
```Bash
World
```

## 深入研究
子串提取技术在早期Unix shell脚本已经存在。到了Bash，这种技术得以进一步优化和强化。别的编程语言也有不同的实现方式，例如Python使用字符串切片。

提取子串有其他的方法，而不仅仅局限于位置和长度。例如，你可以使用正则表达式，或者"tr", "cut"等工具。高级字符串操作可能会用到"sed"和"awk"。

在实现上，Bash使用了C标准库中的' `substr` '函数实现子串提取。这允许Bash快速，有效地处理字符串。

## 参考资料
- Bash文档: [www.gnu.org/software/bash/manual/bash.html](https://www.gnu.org/software/bash/manual/bash.html)
- Bash详细教程, 关于字符串提取: [www.tldp.org/LDP/abs/html/string-manipulation.html](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
- 场景式应用: [www.linuxjournal.com/content/bash-string-manipulation](https://www.linuxjournal.com/content/bash-string-manipulation).