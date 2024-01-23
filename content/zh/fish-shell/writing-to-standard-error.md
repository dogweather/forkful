---
title:                "写入标准错误"
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
标准错误（stderr）是用于记录错误和诊断信息的特殊输出流。程序员这么做以便将错误信息与主要输出分开，方便调试和日志记录。

## 如何：
```Fish Shell
# 写入标准错误的基本方法
echo "This is an error" >&2

# 演示输出
## 正常情况下看不到效果，但如果重定向标准错误流，则会显示：
$ echo "This is an error" >&2 2> error_log.txt
$ cat error_log.txt
This is an error
```

## 深入探讨
在Unix和类Unix系统中，stderr是1970年代以来的一个标准概念。Fish Shell和其他现代shell语言中，通过`>&2`重定向符号可以简便地把输出写入标准错误流。相比之下，你也可以用其他语言实现，例如Bash或是Python，但是Fish的语法更加简洁明了。Fish Shell支持函数、变量和控制流的错误处理策略，这提供了灵活的错误管理方法。

## 参考链接
- [Fish Shell 官方文档](https://fishshell.com/docs/current/index.html)
- [UNIX Standard Streams](https://en.wikipedia.org/wiki/Standard_streams)
