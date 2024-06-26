---
date: 2024-01-26 03:38:01.112583-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u56DE\u5230\u8FC7\u53BB\uFF0CUnix\u547D\u4EE4\
  \u5982 `tr` \u548C `sed` \u662F\u6587\u672C\u5904\u7406\u7684\u4E3B\u8981\u5DE5\u5177\
  \u3002\u4ED6\u4EEC\u4ECA\u5929\u4ECD\u7136\u56E0\u4E3A\u5728\u5904\u7406\u6587\u672C\
  \u8F6C\u6362\uFF08\u5982\u79FB\u9664\u5F15\u53F7\uFF09\u65F6\u7684\u7075\u6D3B\u6027\
  \u548C\u5F3A\u5927\u529F\u80FD\u800C\u88AB\u4F7F\u7528\u3002\u4ED6\u4EEC\u662F\u4EFB\
  \u4F55Shell\u811A\u672C\u7F16\u5199\u8005\u5DE5\u5177\u7BB1\u4E2D\u7684\u5FC5\u5907\
  \u3002\u2026"
lastmod: '2024-04-05T22:51:01.155329-06:00'
model: gpt-4-0125-preview
summary: "\u56DE\u5230\u8FC7\u53BB\uFF0CUnix\u547D\u4EE4\u5982 `tr` \u548C `sed` \u662F\
  \u6587\u672C\u5904\u7406\u7684\u4E3B\u8981\u5DE5\u5177\u3002\u4ED6\u4EEC\u4ECA\u5929\
  \u4ECD\u7136\u56E0\u4E3A\u5728\u5904\u7406\u6587\u672C\u8F6C\u6362\uFF08\u5982\u79FB\
  \u9664\u5F15\u53F7\uFF09\u65F6\u7684\u7075\u6D3B\u6027\u548C\u5F3A\u5927\u529F\u80FD\
  \u800C\u88AB\u4F7F\u7528\u3002\u4ED6\u4EEC\u662F\u4EFB\u4F55Shell\u811A\u672C\u7F16\
  \u5199\u8005\u5DE5\u5177\u7BB1\u4E2D\u7684\u5FC5\u5907\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
weight: 9
---

## 如何操作:
Bash有几种方法可以从字符串中移除引号。这里有一些快速示例：

```Bash
#!/bin/bash

# 使用变量替换来移除单引号和双引号
STRING="\"Hello, World!\""
echo ${STRING//\"}

# 使用 `tr` 删除引号
STRING="'Hello, World!'"
echo $STRING | tr -d "\'"

# 使用 `sed` 删除引号
STRING="\"Hello, World!\""
echo $STRING | sed 's/"//g'
```

示例输出:

```
Hello, World!
Hello, World!
Hello, World!
```

## 深入了解
回到过去，Unix命令如 `tr` 和 `sed` 是文本处理的主要工具。他们今天仍然因为在处理文本转换（如移除引号）时的灵活性和强大功能而被使用。他们是任何Shell脚本编写者工具箱中的必备。

Bash本身已经演化，并且变量替换为小规模字符串操作增加了一层简单性。它免除了向外部二进制文件输出，使你的脚本更加高效。

虽然 `tr` 很适合删除字符，但它不处理更复杂的模式。另一方面，`sed` 使用正则表达式，所以有时可能过于强大，对简单操作来说可能会更慢。

选择这些方法之间取决于你的特定情况。如果你需要去除多种引号，而且你已经在Bash脚本的上下文中，使用变量替换因其简单性而是不费思量的选择。但是，如果你在转换文本流或多行数据，`tr` 和 `sed` 是你的好伙伴。

## 另请参阅:
- GNU Bash手册，特别是关于参数扩展和Shell参数扩展的部分：https://www.gnu.org/software/bash/manual/
- `tr` 命令手册：https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- `sed` 流编辑器概述：https://www.gnu.org/software/sed/manual/sed.html
