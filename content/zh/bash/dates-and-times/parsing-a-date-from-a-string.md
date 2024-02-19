---
aliases:
- /zh/bash/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:41.253237-07:00
description: "\u5728 Bash \u4E2D\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u6D89\
  \u53CA\u4ECE\u6587\u672C\u6570\u636E\u4E2D\u63D0\u53D6\u548C\u8F6C\u6362\u65E5\u671F\
  \u4FE1\u606F\uFF0C\u8F6C\u6362\u6210 Bash \u53EF\u4EE5\u64CD\u4F5C\u6216\u7528\u4E8E\
  \u8FDB\u4E00\u6B65\u5904\u7406\u7684\u683C\u5F0F\u3002\u8FD9\u662F\u811A\u672C\u7F16\
  \u5199\u4E2D\u7684\u4E00\u4E2A\u5E38\u89C1\u9700\u6C42\uFF0C\u4F8B\u5982\u65E5\u5FD7\
  \u6587\u4EF6\u5206\u6790\u3001\u57FA\u4E8E\u65E5\u671F\u6233\u7684\u6587\u4EF6\u7EC4\
  \u7EC7\u6216\u81EA\u52A8\u62A5\u544A\uFF0C\u8FD9\u4F7F\u5F97\u5B83\u6210\u4E3A\u7A0B\
  \u5E8F\u5458\u6709\u6548\u7BA1\u7406\u548C\u5229\u7528\u65F6\u95F4\u6570\u636E\u7684\
  \u91CD\u8981\u6280\u80FD\u3002"
lastmod: 2024-02-18 23:08:59.300550
model: gpt-4-0125-preview
summary: "\u5728 Bash \u4E2D\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u6D89\
  \u53CA\u4ECE\u6587\u672C\u6570\u636E\u4E2D\u63D0\u53D6\u548C\u8F6C\u6362\u65E5\u671F\
  \u4FE1\u606F\uFF0C\u8F6C\u6362\u6210 Bash \u53EF\u4EE5\u64CD\u4F5C\u6216\u7528\u4E8E\
  \u8FDB\u4E00\u6B65\u5904\u7406\u7684\u683C\u5F0F\u3002\u8FD9\u662F\u811A\u672C\u7F16\
  \u5199\u4E2D\u7684\u4E00\u4E2A\u5E38\u89C1\u9700\u6C42\uFF0C\u4F8B\u5982\u65E5\u5FD7\
  \u6587\u4EF6\u5206\u6790\u3001\u57FA\u4E8E\u65E5\u671F\u6233\u7684\u6587\u4EF6\u7EC4\
  \u7EC7\u6216\u81EA\u52A8\u62A5\u544A\uFF0C\u8FD9\u4F7F\u5F97\u5B83\u6210\u4E3A\u7A0B\
  \u5E8F\u5458\u6709\u6548\u7BA1\u7406\u548C\u5229\u7528\u65F6\u95F4\u6570\u636E\u7684\
  \u91CD\u8981\u6280\u80FD\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么和为什么？

在 Bash 中从字符串解析日期涉及从文本数据中提取和转换日期信息，转换成 Bash 可以操作或用于进一步处理的格式。这是脚本编写中的一个常见需求，例如日志文件分析、基于日期戳的文件组织或自动报告，这使得它成为程序员有效管理和利用时间数据的重要技能。

## 如何操作：

Bash 本身在直接日期解析能力上相当有限，通常依赖于像 `date` 和 `awk` 这样的外部工具来进行更精细的操作。以下是如何解析特定格式，然后使用 `date` 命令转换它或执行操作的方法。

**示例 1：** 提取日期字符串并转换成另一种格式。

假设您有一个日期格式为 `yyyy-mm-dd`，并且您想将其转换为 `dd-mm-yyyy`。

```bash
original_date="2023-04-01"
formatted_date=$(date -d $original_date '+%d-%m-%Y')

echo $formatted_date
```

**示例输出：**
```
01-04-2023
```

这使用了 `date` 命令和 `-d` 选项来指定输入的日期字符串，以及 `+%d-%m-%Y` 来格式化输出。

**示例 2：** 使用 `awk` 从结构化文本行中解析日期并转换它。

假设您有一个日志文件行：

```
2023-04-01 12:00:00 用户登陆了
```

您可以使用 `awk` 和 `date` 来提取并转换日期部分。

```bash
log_line="2023-04-01 12:00:00 用户登陆了"
date_part=$(echo $log_line | awk '{print $1}')
formatted_date=$(date -d $date_part "+%A, %B %d, %Y")

echo $formatted_date
```

**示例输出：**
```
星期六, 4月 01, 2023
```

这个示例使用 `awk` 来拆分日志行并提取日期部分（`$1` 代表第一个以空格分隔的字段），然后使用 `date` 来重新格式化它。

### 使用第三方工具

对于更复杂的解析，或当处理多种日期格式时，像 `dateutils` 这样的第三方工具可以非常方便。

**使用 `dateutils` 的示例：**

假设您有一个日期字符串，其格式不是标准格式，例如，`April 01, 2023`。

```bash
original_date="April 01, 2023"
formatted_date=$(dateconv -i "%B %d, %Y" -f "%Y-%m-%d" <<< $original_date)

echo $formatted_date
```

**示例输出：**
```
2023-04-01
```

此命令使用来自 `dateutils` 的 `dateconv`，用 `-i` 指定输入格式，用 `-f` 指定所需的输出格式。`dateutils` 支持广泛的日期和时间格式，使其非常适用于 Bash 脚本中的日期解析任务。
