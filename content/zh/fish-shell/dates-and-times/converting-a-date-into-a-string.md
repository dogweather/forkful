---
date: 2024-01-20 17:36:31.952036-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.278656-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

## How to: (如何操作：)
```Fish Shell
# 当前日期转字符串
set date_str (date "+%Y-%m-%d")
echo $date_str # 输出如: 2023-04-05

# 自定义格式
set custom_date_str (date "+%A, %d %B %Y")
echo $custom_date_str # 输出如: Wednesday, 05 April 2023
```

## Deep Dive (深入探究)
在Unix系操作系统中，`date`命令已经存在很久了，它可以用不同的格式显示日期和时间。Fish Shell使用这个命令通过配置选项来转换日期格式。除了`date`命令，你也可以用其他工具比如`strftime`函数(在某些编程语言中)来实现相同的功能。在Fish Shell中，你直接调用系统的`date`命令，这使得实现起来既直接又高效。

## See Also (参考链接)
- Fish Shell Documentation: https://fishshell.com/docs/current/index.html
- Unix `date` Command: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- strftime Syntax: https://man7.org/linux/man-pages/man3/strftime.3.html
