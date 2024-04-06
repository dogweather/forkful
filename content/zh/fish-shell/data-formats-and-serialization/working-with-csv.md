---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:01.773927-07:00
description: "\u5982\u4F55\u64CD\u4F5C: Fish Shell \u672C\u8EAB\u5E76\u6CA1\u6709\u4E13\
  \u95E8\u4E3A CSV \u64CD\u4F5C\u8BBE\u8BA1\u7684\u5185\u7F6E\u51FD\u6570\u3002\u7136\
  \u800C\uFF0C\u4F60\u53EF\u4EE5\u5229\u7528 Unix \u5B9E\u7528\u5DE5\u5177\u5982 `awk`\u3001\
  `sed`\u548C `cut` \u6765\u8FDB\u884C\u57FA\u672C\u64CD\u4F5C\uFF0C\u6216\u4F7F\u7528\
  \u4E13\u95E8\u7684\u5DE5\u5177\u5982 `csvkit` \u6765\u8FDB\u884C\u66F4\u9AD8\u7EA7\
  \u7684\u4EFB\u52A1\u3002 \u4F7F\u7528 `cut` \u63D0\u53D6\u7B2C\u4E00\u5217\uFF1A\
  ."
lastmod: '2024-03-13T22:44:48.291325-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell \u672C\u8EAB\u5E76\u6CA1\u6709\u4E13\u95E8\u4E3A CSV \u64CD\u4F5C\
  \u8BBE\u8BA1\u7684\u5185\u7F6E\u51FD\u6570\u3002\u7136\u800C\uFF0C\u4F60\u53EF\u4EE5\
  \u5229\u7528 Unix \u5B9E\u7528\u5DE5\u5177\u5982 `awk`\u3001`sed`\u548C `cut` \u6765\
  \u8FDB\u884C\u57FA\u672C\u64CD\u4F5C\uFF0C\u6216\u4F7F\u7528\u4E13\u95E8\u7684\u5DE5\
  \u5177\u5982 `csvkit` \u6765\u8FDB\u884C\u66F4\u9AD8\u7EA7\u7684\u4EFB\u52A1."
title: "\u5904\u7406CSV\u6587\u4EF6"
weight: 37
---

## 如何操作:
Fish Shell 本身并没有专门为 CSV 操作设计的内置函数。然而，你可以利用 Unix 实用工具如 `awk`、`sed`和 `cut` 来进行基本操作，或使用专门的工具如 `csvkit` 来进行更高级的任务。

### 读取 CSV 文件并打印第一列：
使用 `cut` 提取第一列：
```fish
cut -d ',' -f1 data.csv
```
示例输出：
```
Name
Alice
Bob
```

### 根据列值过滤 CSV 行：
使用 `awk` 查找第二列匹配 "42" 的行：
```fish
awk -F, '$2 == "42" { print $0 }' data.csv
```
示例输出：
```
Bob,42,London
```

### 修改 CSV 文件（例如，添加一列）：
使用 `awk` 添加一个静态值 "NewColumn" 的列：
```fish
awk -F, 'BEGIN {OFS=","} {print $0,"NewColumn"}' data.csv > modified.csv
```
`modified.csv` 中的示例输出：
```
Name,Age,City,NewColumn
Alice,30,New York,NewColumn
Bob,42,London,NewColumn
```

### 使用 `csvkit` 进行更高级的操作：
首先，确保你安装了 `csvkit`。如果没有，可以使用 pip 安装：`pip install csvkit`。

**将 CSV 文件转换为 JSON：**
```fish
csvjson data.csv > data.json
```
`data.json` 的示例输出：
```json
[{"Name":"Alice","Age":"30","City":"New York"},{"Name":"Bob","Age":"42","City":"London"}]
```

**使用 `csvkit` 的 `csvgrep` 进行过滤：**
```fish
csvgrep -c 2 -m 42 data.csv
```
此命令使用 `csvkit` 复制过滤任务，针对列 2 查找值 "42"。

总结来说，虽然 Fish Shell 本身可能不提供直接的 CSV 操作能力，但其与 Unix 实用工具的无缝集成以及像 `csvkit` 这样的工具的可用性，为处理 CSV 文件提供了强大的选项。
