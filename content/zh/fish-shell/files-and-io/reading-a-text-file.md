---
date: 2024-01-20 17:54:17.639921-07:00
description: "How to: \u5982\u4F55\u505A\uFF1A \u8BFB\u53D6\u6587\u672C\u6587\u4EF6\
  \u662F\u7F16\u7A0B\u7684\u57FA\u7840\u64CD\u4F5C\u3002\u5728\u65E9\u671FUnix\u7CFB\
  \u7EDF\u4E2D\uFF0C`cat`\u6307\u4EE4\u5C31\u5DF2\u5B58\u5728\uFF0C\u7528\u4E8E\u8FDE\
  \u63A5\u5E76\u6253\u5370\u6587\u4EF6\u5185\u5BB9\u3002Fish\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.560838-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u505A\uFF1A \u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u662F\u7F16\
  \u7A0B\u7684\u57FA\u7840\u64CD\u4F5C\u3002\u5728\u65E9\u671FUnix\u7CFB\u7EDF\u4E2D\
  \uFF0C`cat`\u6307\u4EE4\u5C31\u5DF2\u5B58\u5728\uFF0C\u7528\u4E8E\u8FDE\u63A5\u5E76\
  \u6253\u5370\u6587\u4EF6\u5185\u5BB9\u3002Fish Shell\u7EE7\u627F\u4E86\u8FD9\u4E9B\
  \u547D\u4EE4\uFF0C\u540C\u65F6\u63D0\u4F9B\u4E86\u73B0\u4EE3\u5316\u7684\u8BED\u6CD5\
  \u548C\u5DE5\u5177\u3002\u9664\u4E86`cat`\u548C`less`\uFF0C\u8FD8\u6709\u5982`awk`\u3001\
  `sed`\u7B49\u5F3A\u5927\u7684\u6587\u672C\u5904\u7406\u5DE5\u5177\u3002Fish\u7684\
  `read`\u547D\u4EE4\u4E5F\u53EF\u4EE5\u7528\u6765\u6309\u9700\u89E3\u6790\u6587\u4EF6\
  \u5185\u5BB9\u3002\u7531\u4E8EFish\u81EA\u5E26\u4E30\u5BCC\u7684\u5B57\u7B26\u4E32\
  \u548C\u6587\u4EF6\u5904\u7406\u529F\u80FD\uFF0C\u5927\u591A\u6570\u65F6\u5019\u4F60\
  \u65E0\u987B\u5207\u6362\u5230\u5176\u4ED6\u590D\u6742\u7684\u811A\u672C\u8BED\u8A00\
  \u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

## How to: 如何做：
```Fish Shell
# 使用 cat 读取文件内容
cat example.txt

# 使用 less 分页查看文件内容
less example.txt

# 逐行读取文件并打印每一行
while read -la line
    echo $line
end < example.txt
```
输出示例:
```
Hello, Fish!
This is a sample text file.
```

## Deep Dive 深入探讨
读取文本文件是编程的基础操作。在早期Unix系统中，`cat`指令就已存在，用于连接并打印文件内容。Fish Shell继承了这些命令，同时提供了现代化的语法和工具。除了`cat`和`less`，还有如`awk`、`sed`等强大的文本处理工具。Fish的`read`命令也可以用来按需解析文件内容。由于Fish自带丰富的字符串和文件处理功能，大多数时候你无须切换到其他复杂的脚本语言。

## See Also 参阅链接
- [Fish Shell官网](https://fishshell.com/)
- [Fish Shell文档](https://fishshell.com/docs/current/index.html)
- [Unix文本处理工具](https://en.wikipedia.org/wiki/List_of_Unix_commands#Text_processing)
