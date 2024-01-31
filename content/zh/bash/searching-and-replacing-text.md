---
title:                "搜索和替换文本"
date:                  2024-01-20T17:57:16.876985-07:00
model:                 gpt-4-1106-preview
simple_title:         "搜索和替换文本"

category:             "Bash"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

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
