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

## 为什么

在编程中，我们经常需要对字符串进行大小写转换。使用Bash编程，可以将字符串转换为小写形式，这样可以更方便地对字符串进行处理和比较。

## 如何进行转换

```Bash
# 使用管道和tr命令来转换字符串
echo "HELLO WORLD" | tr '[:upper:]' '[:lower:]'
# 输出：hello world
```
```Bash
# 可以将转换后的字符串赋值给变量
lower_case=$(echo "UPPER CASE" | tr '[:upper:]' '[:lower:]')
echo $lower_case
# 输出：upper case
```
```Bash
# 也可以直接对变量进行转换
original="This Is A MixED cASe"
echo ${original,,}
# 输出：this is a mixed case
```

## 深入了解

在Bash中，可以使用tr命令来进行大小写转换，通过指定每个字符的范围来实现。例如，`'[:upper:]'`表示所有大写字母，`'[:lower:]'`表示所有小写字母。除此之外，Bash还提供了一个内置的变量`${variable,,}`来实现字符串转换为小写形式。

## 参考链接

- Bash官方文档：https://www.gnu.org/software/bash/
- Shell脚本教程：https://www.runoob.com/linux/linux-shell.html
- tr命令详解：https://www.geeksforgeeks.org/tr-command-in-linux-with-examples/

## 查看更多

- 字符串处理方法汇总：https://www.jianshu.com/p/f77efb5ca1c3
- Bash字符串操作指南：https://www.linuxjournal.com/content/bash-string-manipulation
- 菜鸟教程Bash学习指南：https://www.runoob.com/linux/linux-shell.html