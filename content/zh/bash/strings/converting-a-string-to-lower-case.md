---
date: 2024-01-20 17:37:52.728270-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.738749-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
---

{{< edit_this_page >}}

## What & Why?
## 什么和为什么？

在 Bash 中，将字符串转换为小写是改变字符串中所有大写字符为小写的过程。这对数据的标准化处理和比较很有帮助。

## How to:
## 如何操作：

```Bash
# 使用 tr 命令
echo "HELLO WORLD" | tr '[:upper:]' '[:lower:]'
# 输出: hello world

# 或者使用 Bash 内置的,, 扩展
str="HELLO WORLD"
echo "${str,,}"
# 输出: hello world
```

## Deep Dive
## 深入了解：

早期的 Unix 系统中并没有内置的方法来快速转换字符串的大小写。随着时间的推移，各种工具诞生以提供这些功能。`tr` 是一个非常早期的文本处理工具，用于字符替换和删除。它非常适合用于大小写转换。

Bash 自版本 4.0 起提供了内置的方法。使用双小写逗号 `,,` 进行小写转换或双大写逗号 `^^` 还原大写，常见于参数扩展中。

除了 `tr` 和 Bash 内置特性，有些人可能也会用 `awk` 或 `sed`，但对于单纯的大小写转换，这些方法更笨重，不建议使用。

在执行字符串小写转换时，保持字符集和区域设置（locale）的一致很重要。Bash 的大小写转换是基于当前区域设置进行的，不同的区域设置可能导致不同的转换结果。

## See Also
## 参考链接：

- Bash 手册: https://www.gnu.org/software/bash/manual/
- `tr` 命令帮助: `man tr`
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
