---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:08.292366-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Fish Shell\u4E2D\uFF0C\u4F60\u6CA1\
  \u6709\u4E13\u95E8\u4E3A\u4ECE\u5B57\u7B26\u4E32\u4E2D\u89E3\u6790\u65E5\u671F\u8BBE\
  \u8BA1\u7684\u5185\u7F6E\u547D\u4EE4\u3002\u76F8\u53CD\uFF0C\u4F60\u9700\u8981\u4F9D\
  \u9760\u5916\u90E8\u5B9E\u7528\u7A0B\u5E8F\uFF0C\u5982`date`\uFF08\u5728Linux\u548C\
  macOS\u4E2D\u53EF\u7528\uFF09\u6216\u5229\u7528\u6D41\u884C\u7684\u7B2C\u4E09\u65B9\
  \u5DE5\u5177\uFF0C\u5982`GNU date`\uFF0C\u4EE5\u8FDB\u884C\u66F4\u590D\u6742\u7684\
  \u89E3\u6790\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u8FDB\u884C\u64CD\u4F5C\u7684\u65B9\
  \u6CD5\uFF1A **\u5728Fish\u4E2D\u4F7F\u7528`date`\uFF1A**\u2026"
lastmod: '2024-04-05T21:53:48.552400-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

## 如何操作：
在Fish Shell中，你没有专门为从字符串中解析日期设计的内置命令。相反，你需要依靠外部实用程序，如`date`（在Linux和macOS中可用）或利用流行的第三方工具，如`GNU date`，以进行更复杂的解析。以下是如何进行操作的方法：

**在Fish中使用`date`：**

要解析格式为"YYYY-MM-DD"的日期字符串，你可以使用`date`命令，后面跟`-d`（或对于GNU date是`--date`）选项，然后是字符串。`+`选项用于格式化输出。

```fish
set date_str "2023-04-01"
date -d $date_str +"%A, %d %B %Y"
# 输出：Saturday, 01 April 2023
```

对于macOS（需要对`-j`和`-f`标志使用不同的格式）：

```fish
set date_str "2023-04-01"
date -j -f "%Y-%m-%d" $date_str +"%A, %d %B %Y"
# 输出：Saturday, 01 April 2023
```

**使用GNU `date`进行复杂解析：**

GNU `date`在字符串格式方面更灵活。它可以自动检测许多常见的日期字符串格式，而无需显式指定输入格式：

```fish
set complex_date_str "April 1, 2023 14:00"
date -d "$complex_date_str" '+%Y-%m-%d %H:%M:%S'
# 输出：2023-04-01 14:00:00
```

然而，当处理可能无法自动识别的日期字符串，或者需要对输入格式进行精确控制时，GNU `date`并不直接支持指定输入格式。在这种情况下，考虑预处理字符串或使用另一个为更复杂的日期解析例程设计的工具。
