---
aliases:
- /zh/fish-shell/extracting-substrings/
date: 2024-01-20 17:45:51.273287-07:00
description: "\u4EC0\u4E48\u662F\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\uFF1F\u7B80\u5355\
  \u5730\u8BF4\uFF0C\u5C31\u662F\u4ECE\u4E00\u4E2A\u66F4\u957F\u7684\u5B57\u7B26\u4E32\
  \u4E2D\u53D6\u51FA\u4E00\u90E8\u5206\u3002\u7A0B\u5E8F\u5458\u4E3A\u4EC0\u4E48\u8981\
  \u8FD9\u4E48\u505A\uFF1F\u56E0\u4E3A\u5728\u5904\u7406\u6587\u672C\u65F6\uFF0C\u6211\
  \u4EEC\u7ECF\u5E38\u9700\u8981\u8BBF\u95EE\u3001\u4FEE\u6539\u6216\u5206\u6790\u5B57\
  \u7B26\u4E32\u7684\u7279\u5B9A\u90E8\u5206\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.507507
model: gpt-4-1106-preview
summary: "\u4EC0\u4E48\u662F\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\uFF1F\u7B80\u5355\
  \u5730\u8BF4\uFF0C\u5C31\u662F\u4ECE\u4E00\u4E2A\u66F4\u957F\u7684\u5B57\u7B26\u4E32\
  \u4E2D\u53D6\u51FA\u4E00\u90E8\u5206\u3002\u7A0B\u5E8F\u5458\u4E3A\u4EC0\u4E48\u8981\
  \u8FD9\u4E48\u505A\uFF1F\u56E0\u4E3A\u5728\u5904\u7406\u6587\u672C\u65F6\uFF0C\u6211\
  \u4EEC\u7ECF\u5E38\u9700\u8981\u8BBF\u95EE\u3001\u4FEE\u6539\u6216\u5206\u6790\u5B57\
  \u7B26\u4E32\u7684\u7279\u5B9A\u90E8\u5206\u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## What & Why?
什么是提取子字符串？简单地说，就是从一个更长的字符串中取出一部分。程序员为什么要这么做？因为在处理文本时，我们经常需要访问、修改或分析字符串的特定部分。

## How to:
```
Fish Shell 示例：

# 假设我们有一个字符串
set str "Fish是一个现代化的交互式Shell"

# 提取"一个"之后的内容
echo $str | string sub --start (string match -r -i -n "一个" $str)[1]

# 输出应该是："现代化的交互式Shell"

# 定义起始和结束位置
set start 7
set length 6

# 提取特定范围的子字符串
echo $str | string sub -s $start -l $length

# 输出应该是："一个现代化"
```

## Deep Dive
提取子字符串这个概念在计算机编程的需要由来已久。从UNIX Shell脚本到现代编程语言，这个功能始终重要。在Fish Shell里，`string` 命令的 `sub` 子命令允许你方便地进行这项操作。

与传统的Bash Shell相比，Fish Shell的设计是现代的，语法明确，更易读写。它的 `string` 命令比Bash的内置字符串处理功能强大得多。Bash通常需要使用外部工具如 `cut`、`awk` 或 `sed`，而Fish则集成了这些功能。

实现细节上，Fish使用了内部的 `string` 命令来处理字符串操作，它与其他命令一样，内建支持管道和参数，使得操作更加直观和灵活。

## See Also
- Fish官方文档：[string](https://fishshell.com/docs/current/cmds/string.html)
- Fish Shell教程和例子：[Fish Tutorial](https://fishshell.com/docs/current/tutorial.html)
- 更多字符串操作细节：[Fish Shell 文档 - 字符串](https://fishshell.com/docs/current/index.html#expand-variable)
