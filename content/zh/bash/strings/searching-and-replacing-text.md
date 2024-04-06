---
date: 2024-01-20 17:57:16.876985-07:00
description: "How to: \u600E\u4E48\u505A `sed` \u547D\u4EE4\u6765\u81EA\u4E8E `stream\
  \ editor` \u7684\u7F29\u5199\uFF0C\u662F Unix \u7CFB\u7EDF\u4E2D\u5F88\u4F20\u7EDF\
  \u7684\u6587\u672C\u5904\u7406\u5DE5\u5177\uFF0C\u4E8E 1974 \u5E74\u9996\u6B21\u53D1\
  \u5E03\u3002\u9664\u4E86 `sed`\uFF0C\u8FD8\u6709 `awk`, `grep`, `perl` \u7B49\u5DE5\
  \u5177\u4E5F\u80FD\u505A\u8FD9\u5DE5\u4F5C\u3002`sed`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.152263-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u4E48\u505A `sed` \u547D\u4EE4\u6765\u81EA\u4E8E `stream editor`\
  \ \u7684\u7F29\u5199\uFF0C\u662F Unix \u7CFB\u7EDF\u4E2D\u5F88\u4F20\u7EDF\u7684\
  \u6587\u672C\u5904\u7406\u5DE5\u5177\uFF0C\u4E8E 1974 \u5E74\u9996\u6B21\u53D1\u5E03\
  \u3002\u9664\u4E86 `sed`\uFF0C\u8FD8\u6709 `awk`, `grep`, `perl` \u7B49\u5DE5\u5177\
  \u4E5F\u80FD\u505A\u8FD9\u5DE5\u4F5C\u3002`sed` \u4F7F\u7528\u6B63\u5219\u8868\u8FBE\
  \u5F0F\u6765\u5339\u914D\u6587\u672C\uFF0C\u5728\u5DE5\u4F5C\u4E2D\u975E\u5E38\u7075\
  \u6D3B\u4F46\u53EF\u80FD\u590D\u6742\u3002\u76F4\u63A5\u5728\u6587\u4EF6\u4E2D\u66FF\
  \u6362\u6587\u672C (`sed -i`) \u4E4B\u524D\u52A1\u5FC5\u5907\u4EFD\uFF0C\u4EE5\u907F\
  \u514D\u610F\u5916\u66F4\u6539\u3002"
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
