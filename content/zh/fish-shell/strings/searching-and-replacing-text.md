---
date: 2024-01-20 17:58:00.634899-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) Fish Shell \u63D0\u4F9B\u5185\
  \u5EFA\u7684 `string` \u547D\u4EE4\u6765\u5904\u7406\u6587\u672C\u64CD\u4F5C\uFF0C\
  \u5176\u4E2D\u5305\u62EC\u641C\u7D22\u548C\u66FF\u6362\u529F\u80FD\u3002\u6BD4\u8D77\
  \u4F20\u7EDF\u7684 `sed` \u547D\u4EE4\uFF0C`string` \u66F4\u76F4\u89C2\u7B80\u6D01\
  \u3002`string replace` \u652F\u6301\u57FA\u672C\u7684\u66FF\u6362\u64CD\u4F5C\u548C\
  \u6B63\u5219\u8868\u8FBE\u5F0F\uFF0C\u53EF\u4EE5\u6267\u884C\u5C40\u90E8\u6216\u5168\
  \u5C40\u66FF\u6362\u3002\u5168\u5C40\u66FF\u6362\u85C9\u7531\u6DFB\u52A0 `-a`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.522039-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) Fish Shell \u63D0\u4F9B\u5185\u5EFA\u7684\
  \ `string` \u547D\u4EE4\u6765\u5904\u7406\u6587\u672C\u64CD\u4F5C\uFF0C\u5176\u4E2D\
  \u5305\u62EC\u641C\u7D22\u548C\u66FF\u6362\u529F\u80FD\u3002\u6BD4\u8D77\u4F20\u7EDF\
  \u7684 `sed` \u547D\u4EE4\uFF0C`string` \u66F4\u76F4\u89C2\u7B80\u6D01\u3002`string\
  \ replace` \u652F\u6301\u57FA\u672C\u7684\u66FF\u6362\u64CD\u4F5C\u548C\u6B63\u5219\
  \u8868\u8FBE\u5F0F\uFF0C\u53EF\u4EE5\u6267\u884C\u5C40\u90E8\u6216\u5168\u5C40\u66FF\
  \u6362\u3002\u5168\u5C40\u66FF\u6362\u85C9\u7531\u6DFB\u52A0 `-a` \u53C2\u6570\u5B9E\
  \u73B0\uFF0C\u6307\u5B9A `-i` \u53EF\u8FDB\u884C\u4E0D\u533A\u5206\u5927\u5C0F\u5199\
  \u7684\u66FF\u6362\u3002\u800C\u5386\u53F2\u4E0A\uFF0CUNIX \u7CFB\u7EDF\u4E2D\u7ECF\
  \u5E38\u501F\u52A9 `sed` \u6216 `awk` \u5B9E\u73B0\u641C\u7D22\u66FF\u6362\uFF0C\
  \u800C\u73B0\u5728 Fish Shell \u7684 `string` \u63D0\u4F9B\u4E86\u4E00\u4E2A\u73B0\
  \u4EE3\u7684\u66FF\u4EE3\u65B9\u6848\u3002"
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
weight: 10
---

## How to: (如何操作：)
```Fish Shell
# 查找 "hello" 并替换为 "hi"
echo "hello world" | string replace "hello" "hi"
# 输出: hi world

# 全局替换，所有的 "o" 都换成 "0"
echo "lorem ipsum dolor sit amet" | string replace -a "o" "0"
# 输出: l0rem ipsum d0l0r sit amet
```

## Deep Dive (深入了解)
Fish Shell 提供内建的 `string` 命令来处理文本操作，其中包括搜索和替换功能。比起传统的 `sed` 命令，`string` 更直观简洁。`string replace` 支持基本的替换操作和正则表达式，可以执行局部或全局替换。全局替换藉由添加 `-a` 参数实现，指定 `-i` 可进行不区分大小写的替换。而历史上，UNIX 系统中经常借助 `sed` 或 `awk` 实现搜索替换，而现在 Fish Shell 的 `string` 提供了一个现代的替代方案。

## See Also (另请参见)
- Fish Shell 官方文档关于 `string` 命令的部分：https://fishshell.com/docs/current/cmds/string.html
- POSIX 标准 `sed` 命令教程：https://www.gnu.org/software/sed/manual/sed.html
- 正则表达式入门：https://www.regular-expressions.info/tutorial.html
