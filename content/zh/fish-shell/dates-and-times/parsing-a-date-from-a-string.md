---
title:                "从字符串解析日期"
date:                  2024-02-03T19:14:08.292366-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串解析日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
