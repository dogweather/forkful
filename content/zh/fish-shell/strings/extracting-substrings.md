---
date: 2024-01-20 17:45:51.273287-07:00
description: "How to: \u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u8FD9\u4E2A\u6982\u5FF5\
  \u5728\u8BA1\u7B97\u673A\u7F16\u7A0B\u7684\u9700\u8981\u7531\u6765\u5DF2\u4E45\u3002\
  \u4ECEUNIX Shell\u811A\u672C\u5230\u73B0\u4EE3\u7F16\u7A0B\u8BED\u8A00\uFF0C\u8FD9\
  \u4E2A\u529F\u80FD\u59CB\u7EC8\u91CD\u8981\u3002\u5728Fish Shell\u91CC\uFF0C`string`\
  \ \u547D\u4EE4\u7684 `sub` \u5B50\u547D\u4EE4\u5141\u8BB8\u4F60\u65B9\u4FBF\u5730\
  \u8FDB\u884C\u8FD9\u9879\u64CD\u4F5C\u3002 \u4E0E\u4F20\u7EDF\u7684Bash Shell\u76F8\
  \u6BD4\uFF0CFish\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.389053-06:00'
model: gpt-4-1106-preview
summary: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u8FD9\u4E2A\u6982\u5FF5\u5728\u8BA1\
  \u7B97\u673A\u7F16\u7A0B\u7684\u9700\u8981\u7531\u6765\u5DF2\u4E45\u3002\u4ECEUNIX\
  \ Shell\u811A\u672C\u5230\u73B0\u4EE3\u7F16\u7A0B\u8BED\u8A00\uFF0C\u8FD9\u4E2A\u529F\
  \u80FD\u59CB\u7EC8\u91CD\u8981\u3002\u5728Fish Shell\u91CC\uFF0C`string` \u547D\u4EE4\
  \u7684 `sub` \u5B50\u547D\u4EE4\u5141\u8BB8\u4F60\u65B9\u4FBF\u5730\u8FDB\u884C\u8FD9\
  \u9879\u64CD\u4F5C\u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

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
