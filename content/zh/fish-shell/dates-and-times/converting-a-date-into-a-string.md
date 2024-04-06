---
date: 2024-01-20 17:36:31.952036-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728Unix\u7CFB\u64CD\u4F5C\
  \u7CFB\u7EDF\u4E2D\uFF0C`date`\u547D\u4EE4\u5DF2\u7ECF\u5B58\u5728\u5F88\u4E45\u4E86\
  \uFF0C\u5B83\u53EF\u4EE5\u7528\u4E0D\u540C\u7684\u683C\u5F0F\u663E\u793A\u65E5\u671F\
  \u548C\u65F6\u95F4\u3002Fish Shell\u4F7F\u7528\u8FD9\u4E2A\u547D\u4EE4\u901A\u8FC7\
  \u914D\u7F6E\u9009\u9879\u6765\u8F6C\u6362\u65E5\u671F\u683C\u5F0F\u3002\u9664\u4E86\
  `date`\u547D\u4EE4\uFF0C\u4F60\u4E5F\u53EF\u4EE5\u7528\u5176\u4ED6\u5DE5\u5177\u6BD4\
  \u5982`strftime`\u51FD\u6570(\u5728\u67D0\u4E9B\u7F16\u7A0B\u8BED\u8A00\u4E2D)\u6765\
  \u5B9E\u73B0\u76F8\u540C\u7684\u529F\u80FD\u3002\u5728Fish\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.554511-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5728Unix\u7CFB\u64CD\u4F5C\u7CFB\u7EDF\
  \u4E2D\uFF0C`date`\u547D\u4EE4\u5DF2\u7ECF\u5B58\u5728\u5F88\u4E45\u4E86\uFF0C\u5B83\
  \u53EF\u4EE5\u7528\u4E0D\u540C\u7684\u683C\u5F0F\u663E\u793A\u65E5\u671F\u548C\u65F6\
  \u95F4\u3002Fish Shell\u4F7F\u7528\u8FD9\u4E2A\u547D\u4EE4\u901A\u8FC7\u914D\u7F6E\
  \u9009\u9879\u6765\u8F6C\u6362\u65E5\u671F\u683C\u5F0F\u3002\u9664\u4E86`date`\u547D\
  \u4EE4\uFF0C\u4F60\u4E5F\u53EF\u4EE5\u7528\u5176\u4ED6\u5DE5\u5177\u6BD4\u5982`strftime`\u51FD\
  \u6570(\u5728\u67D0\u4E9B\u7F16\u7A0B\u8BED\u8A00\u4E2D)\u6765\u5B9E\u73B0\u76F8\
  \u540C\u7684\u529F\u80FD\u3002\u5728Fish Shell\u4E2D\uFF0C\u4F60\u76F4\u63A5\u8C03\
  \u7528\u7CFB\u7EDF\u7684`date`\u547D\u4EE4\uFF0C\u8FD9\u4F7F\u5F97\u5B9E\u73B0\u8D77\
  \u6765\u65E2\u76F4\u63A5\u53C8\u9AD8\u6548\u3002"
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
