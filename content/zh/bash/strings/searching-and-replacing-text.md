---
date: 2024-01-20 17:57:16.876985-07:00
description: "How to: \u600E\u4E48\u505A \u8F93\u51FA\u793A\u4F8B\uFF08sample output\uFF09\
  ."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.246362-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u4E48\u505A \u8F93\u51FA\u793A\u4F8B\uFF08sample output\uFF09."
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
weight: 10
---

## How to: 怎么做
```Bash
# 基本的搜索和替换
sed 's/old-text/new-text/g' filename

# 替换后保存到新文件
sed 's/old-text/new-text/g' filename > new-filename 

# 直接修改原文件里的文本（小心使用）
sed -i 's/old-text/new-text/g' filename
```
输出示例（sample output）:
```Bash
# 假设文件里的内容是 "Hello old-text!"
sed 's/old-text/new-text/g' filename
# 输出: "Hello new-text!"
```

## Deep Dive 深入探索
`sed` 命令来自于 `stream editor` 的缩写，是 Unix 系统中很传统的文本处理工具，于 1974 年首次发布。除了 `sed`，还有 `awk`, `grep`, `perl` 等工具也能做这工作。`sed` 使用正则表达式来匹配文本，在工作中非常灵活但可能复杂。直接在文件中替换文本 (`sed -i`) 之前务必备份，以避免意外更改。

## See Also 参看其他
- GNU sed 手册: https://www.gnu.org/software/sed/manual/sed.html
- Learn sed with hands-on examples: http://www.grymoire.com/Unix/Sed.html
- Regular Expressions 教程: https://www.regular-expressions.info/
