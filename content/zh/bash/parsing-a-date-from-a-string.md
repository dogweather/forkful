---
title:                "从字符串解析日期"
date:                  2024-01-20T15:34:45.721862-07:00
html_title:           "Arduino: 从字符串解析日期"
simple_title:         "从字符串解析日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
解析日期就是从字符串中提取日期信息。程序员这样做是为了自动化处理日期数据，进行排序、比较或转换。

## How to (如何做)：
使用 `date` 命令与 `-d` 选项解析字符串中的日期：

```Bash
# 解析标准日期字符串
parsed_date=$(date -d '2023-03-15' '+%Y-%m-%d')
echo $parsed_date
# 输出: 2023-03-15

# 将一个非标准格式的日期字符串转换为其他格式
date_input="15 March 2023"
formatted_date=$(date -d "$date_input" '+%Y-%m-%d')
echo $formatted_date
# 输出: 2023-03-15
```
注意：Bash 本身没有内建的日期解析功能，依赖于外部工具，如 `date`。

## Deep Dive (深入探索)：
日期字符串解析可以追溯到Unix时代，那时的工具已经能够处理简单的日期和时间任务。`date` 命令是最常见的日期解析工具，几乎在每个类Unix系统中可用。

替代方案：除了 `date`，你还可以使用 `awk`、`sed` 或 `perl` 等工具来解析日期。

实现细节：`date` 命令使用了很多标准和非标准格式识别日期字符串。它还允许用户自定义输出格式，这可以通过 '+' 后跟格式字符串来实现。

## See Also (另请参阅)：
- GNU Coreutils `date` 手册: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/dates.html
- 关于 `awk`、`sed` 和 `perl` 的日期处理示例: https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/
