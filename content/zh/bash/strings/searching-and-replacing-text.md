---
date: 2024-01-20 17:57:16.876985-07:00
description: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u662F\u5728\u6587\u4EF6\u4E2D\
  \u627E\u7279\u5B9A\u5B57\u7B26\u6216\u5B57\u7B26\u4E32\uFF0C\u7136\u540E\u6362\u6210\
  \u522B\u7684\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u901A\u5E38\u662F\
  \u4E3A\u4E86\u5FEB\u901F\u4FEE\u6B63\u9519\u8BEF\u3001\u66F4\u65B0\u4FE1\u606F\u6216\
  \u6539\u8FDB\u4EE3\u7801\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.942665-06:00'
model: gpt-4-1106-preview
summary: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u662F\u5728\u6587\u4EF6\u4E2D\
  \u627E\u7279\u5B9A\u5B57\u7B26\u6216\u5B57\u7B26\u4E32\uFF0C\u7136\u540E\u6362\u6210\
  \u522B\u7684\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u901A\u5E38\u662F\
  \u4E3A\u4E86\u5FEB\u901F\u4FEE\u6B63\u9519\u8BEF\u3001\u66F4\u65B0\u4FE1\u606F\u6216\
  \u6539\u8FDB\u4EE3\u7801\u3002."
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
weight: 10
---

## What & Why? 什么和为什么?
搜索和替换文本是在文件中找特定字符或字符串，然后换成别的内容。程序员这么做通常是为了快速修正错误、更新信息或改进代码。

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
