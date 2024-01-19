---
title:                "删除匹配模式的字符"
html_title:           "Java: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 什么以及为什么？

删除匹配模式字符是一项运算，该运算会移除字符串中与特定模式相匹配的字符。程序员经常进行此操作以改变、清理或简化数据。

## 怎么实现：

Bash中的删除字符匹配操作主要通过`tr -d`命令实现。下面是一些例子：

```Bash
# 删除所有数字
echo "A1B2C3D4" | tr -d '0-9'
输出：ABCD

# 删除所有小写字母
echo "HelloWorld" | tr -d 'a-z'
输出：HW
```

## 进一步深入：

1. 历史背景：`tr`命令首次在Unix V3 (发布于1973年)中出现，早期主要用于字符转换和删除操作。

2. 替代方案：`sed`命令是另一种常用工具，可执行更复杂的操作。例如，使用`sed`删除所有数字：

```Bash
echo "A1B2C3D4" | sed 's/[0-9]//g'
输出：ABCD
```

3. 实现细节：`tr`通过从标准输入读取字符，并对每个字符执行指定的操作。`-d`选项将执行删除函项，只对列表中出现的字符进行操作。

## 更多相关资料：

- GNU Bash官方手册： [https://www.gnu.org/software/bash/](https://www.gnu.org/software/bash/)
- TR命令示例使用： [https://www.geeksforgeeks.org/tr-command-unixlinux-examples/](https://www.geeksforgeeks.org/tr-command-unixlinux-examples/)
- `Sed`命令教程： [https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/)