---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:08.292366-07:00
description: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u89E3\u6790\u65E5\u671F\u6D89\u53CA\u63D0\
  \u53D6\u7F16\u7801\u5728\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\u4FE1\u606F\uFF0C\
  \u5E76\u5C06\u5176\u8F6C\u6362\u6210\u7F16\u7A0B\u73AF\u5883\u53EF\u4EE5\u8BC6\u522B\
  \u548C\u64CD\u4F5C\u7684\u7ED3\u6784\u5316\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u542F\u7528\u8BF8\u5982\u65E5\u671F\u6BD4\u8F83\u3001\
  \u7B97\u672F\u8FD0\u7B97\u3001\u683C\u5F0F\u5316\u548C\u672C\u5730\u5316\u7B49\u64CD\
  \u4F5C\uFF0C\u8FD9\u5BF9\u4E8E\u5728\u8F6F\u4EF6\u4E2D\u9AD8\u6548\u5904\u7406\u65E5\
  \u7A0B\u5B89\u6392\u3001\u65F6\u95F4\u6233\u548C\u5386\u53F2\u6570\u636E\u81F3\u5173\
  \u91CD\u8981\u3002"
lastmod: '2024-03-11T00:14:22.084501-06:00'
model: gpt-4-0125-preview
summary: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u89E3\u6790\u65E5\u671F\u6D89\u53CA\u63D0\
  \u53D6\u7F16\u7801\u5728\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\u4FE1\u606F\uFF0C\
  \u5E76\u5C06\u5176\u8F6C\u6362\u6210\u7F16\u7A0B\u73AF\u5883\u53EF\u4EE5\u8BC6\u522B\
  \u548C\u64CD\u4F5C\u7684\u7ED3\u6784\u5316\u683C\u5F0F\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u542F\u7528\u8BF8\u5982\u65E5\u671F\u6BD4\u8F83\u3001\
  \u7B97\u672F\u8FD0\u7B97\u3001\u683C\u5F0F\u5316\u548C\u672C\u5730\u5316\u7B49\u64CD\
  \u4F5C\uFF0C\u8FD9\u5BF9\u4E8E\u5728\u8F6F\u4EF6\u4E2D\u9AD8\u6548\u5904\u7406\u65E5\
  \u7A0B\u5B89\u6392\u3001\u65F6\u95F4\u6233\u548C\u5386\u53F2\u6570\u636E\u81F3\u5173\
  \u91CD\u8981\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？
从字符串中解析日期涉及提取编码在字符串中的日期信息，并将其转换成编程环境可以识别和操作的结构化格式。程序员这样做是为了启用诸如日期比较、算术运算、格式化和本地化等操作，这对于在软件中高效处理日程安排、时间戳和历史数据至关重要。

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
