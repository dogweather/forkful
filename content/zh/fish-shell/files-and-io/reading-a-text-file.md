---
date: 2024-01-20 17:54:17.639921-07:00
description: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\uFF0C\u5C31\u662F\u5C06\u6587\u4EF6\
  \u5185\u5BB9\u52A0\u8F7D\u5230\u7A0B\u5E8F\u4E2D\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\
  \u505A\u662F\u4E3A\u4E86\u5904\u7406\u4FE1\u606F\uFF0C\u65E0\u8BBA\u662F\u914D\u7F6E\
  \u6570\u636E\u3001\u7528\u6237\u8F93\u5165\u8FD8\u662F\u65E5\u5FD7\u4FE1\u606F\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:07.342908
model: gpt-4-1106-preview
summary: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\uFF0C\u5C31\u662F\u5C06\u6587\u4EF6\
  \u5185\u5BB9\u52A0\u8F7D\u5230\u7A0B\u5E8F\u4E2D\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\
  \u505A\u662F\u4E3A\u4E86\u5904\u7406\u4FE1\u606F\uFF0C\u65E0\u8BBA\u662F\u914D\u7F6E\
  \u6570\u636E\u3001\u7528\u6237\u8F93\u5165\u8FD8\u662F\u65E5\u5FD7\u4FE1\u606F\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## What & Why? 什么 & 为什么？
读取文本文件，就是将文件内容加载到程序中。程序员这么做是为了处理信息，无论是配置数据、用户输入还是日志信息。

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
