---
date: 2024-01-20 17:36:31.952036-07:00
description: "\u8F6C\u6362\u65E5\u671F\u5230\u5B57\u7B26\u4E32\u5C31\u662F\u628A\u65E5\
  \u671F\u4ECE\u5176\u539F\u59CB\u683C\u5F0F\u53D8\u6210\u6587\u672C\u5F62\u5F0F\u3002\
  \u8FD9\u8BA9\u7A0B\u5E8F\u5458\u80FD\u66F4\u7075\u6D3B\u5730\u5904\u7406\u548C\u663E\
  \u793A\u65E5\u671F\uFF0C\u6BD4\u5982\u5728\u65E5\u5FD7\u6587\u4EF6\u6216\u7528\u6237\
  \u754C\u9762\u4E2D\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.837030-07:00'
model: gpt-4-1106-preview
summary: "\u8F6C\u6362\u65E5\u671F\u5230\u5B57\u7B26\u4E32\u5C31\u662F\u628A\u65E5\
  \u671F\u4ECE\u5176\u539F\u59CB\u683C\u5F0F\u53D8\u6210\u6587\u672C\u5F62\u5F0F\u3002\
  \u8FD9\u8BA9\u7A0B\u5E8F\u5458\u80FD\u66F4\u7075\u6D3B\u5730\u5904\u7406\u548C\u663E\
  \u793A\u65E5\u671F\uFF0C\u6BD4\u5982\u5728\u65E5\u5FD7\u6587\u4EF6\u6216\u7528\u6237\
  \u754C\u9762\u4E2D\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
转换日期到字符串就是把日期从其原始格式变成文本形式。这让程序员能更灵活地处理和显示日期，比如在日志文件或用户界面中。

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
