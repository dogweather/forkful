---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:44.128508-07:00
description: "\u5728 Bash \u4E2D\u5904\u7406 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\
  \uFF09\u6587\u4EF6\u662F\u5173\u4E8E\u5904\u7406\u548C\u64CD\u4F5C\u4EE5\u7EAF\u6587\
  \u672C\u683C\u5F0F\u5B58\u50A8\u7684\u8868\u683C\u6570\u636E\u3002\u8FD9\u5BF9\u7A0B\
  \u5E8F\u5458\u6765\u8BF4\u81F3\u5173\u91CD\u8981\uFF0C\u56E0\u4E3A\u5B83\u5141\u8BB8\
  \u76F4\u63A5\u4ECE\u547D\u4EE4\u884C\u81EA\u52A8\u5316\u6570\u636E\u8F6C\u6362\u3001\
  \u5206\u6790\u548C\u96C6\u6210\u4EFB\u52A1\uFF0C\u65E0\u9700\u66F4\u91CD\u91CF\u7EA7\
  \u7684\u5DE5\u5177\u6216\u7F16\u7A0B\u73AF\u5883\u3002"
lastmod: '2024-03-13T22:44:47.989036-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Bash \u4E2D\u5904\u7406 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\
  \u6587\u4EF6\u662F\u5173\u4E8E\u5904\u7406\u548C\u64CD\u4F5C\u4EE5\u7EAF\u6587\u672C\
  \u683C\u5F0F\u5B58\u50A8\u7684\u8868\u683C\u6570\u636E\u3002\u8FD9\u5BF9\u7A0B\u5E8F\
  \u5458\u6765\u8BF4\u81F3\u5173\u91CD\u8981\uFF0C\u56E0\u4E3A\u5B83\u5141\u8BB8\u76F4\
  \u63A5\u4ECE\u547D\u4EE4\u884C\u81EA\u52A8\u5316\u6570\u636E\u8F6C\u6362\u3001\u5206\
  \u6790\u548C\u96C6\u6210\u4EFB\u52A1\uFF0C\u65E0\u9700\u66F4\u91CD\u91CF\u7EA7\u7684\
  \u5DE5\u5177\u6216\u7F16\u7A0B\u73AF\u5883\u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 Bash 中处理 CSV（逗号分隔值）文件是关于处理和操作以纯文本格式存储的表格数据。这对程序员来说至关重要，因为它允许直接从命令行自动化数据转换、分析和集成任务，无需更重量级的工具或编程环境。

## 如何操作：

**逐行读取 CSV 文件**

```bash
while IFS=, read -r column1 column2 column3
do
  echo "列 1: $column1, 列 2: $column2, 列 3: $column3"
done < sample.csv
```

*示例输出：*

```
列 1: id, 列 2: name, 列 3: email
...
```

**基于条件筛选 CSV 行**

使用 `awk`，可以轻松筛选行。例如，找到第二列等于 "Alice" 的行：

```bash
awk -F, '$2 == "Alice" { print $0 }' sample.csv
```

**修改列值**

将第二列更改为大写：

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' sample.csv
```

**根据列对 CSV 文件排序**

您可以根据，比如说，第三列（数字化）对 CSV 文件进行排序：

```bash
sort -t, -k3,3n sample.csv
```

**使用 `csvkit` 进行更复杂的任务**

`csvkit` 是一套用于转换和处理 CSV 的命令行工具。可以通过 pip 安装。

将 JSON 文件转换为 CSV：

```bash
in2csv data.json > data.csv
```

使用 SQL 查询 CSV 文件：

```bash
csvsql --query "SELECT name FROM sample WHERE id = 10" sample.csv
```

*注意：安装 `csvkit` 需要 Python，并且可以使用 `pip install csvkit` 来完成。*
