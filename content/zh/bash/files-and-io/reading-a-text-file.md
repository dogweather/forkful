---
date: 2024-01-20 17:53:59.734228-07:00
description: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5C31\u662F\u628A\u5B58\u50A8\u5728\
  \u6587\u4EF6\u4E2D\u7684\u6587\u672C\u4FE1\u606F\u52A0\u8F7D\u5230\u7A0B\u5E8F\u4E2D\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5904\u7406\u6570\u636E\
  \u3001\u914D\u7F6E\u7A0B\u5E8F\u6216\u8005\u8BFB\u53D6\u6765\u6E90\u4FE1\u606F\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.779262-06:00'
model: gpt-4-1106-preview
summary: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u5C31\u662F\u628A\u5B58\u50A8\u5728\
  \u6587\u4EF6\u4E2D\u7684\u6587\u672C\u4FE1\u606F\u52A0\u8F7D\u5230\u7A0B\u5E8F\u4E2D\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5904\u7406\u6570\u636E\
  \u3001\u914D\u7F6E\u7A0B\u5E8F\u6216\u8005\u8BFB\u53D6\u6765\u6E90\u4FE1\u606F\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
读取文本文件就是把存储在文件中的文本信息加载到程序中。程序员这么做是为了处理数据、配置程序或者读取来源信息。

## How to: (如何操作：)
```Bash
# Reading a whole file with 'cat'
cat myfile.txt

# Reading line by line using a while loop
while IFS= read -r line
do
  echo "Line: $line"
done < "myfile.txt"
```
输出示例：
```
Line: 第一行内容
Line: 第二行内容
...
```

## Deep Dive (深入钻研)
历史上，文本文件始终是信息交换的基础。Unix哲学强调使用简单的、文本形式的数据交换格式。Bash 借此传统，内置了许多读取文本文件的工具，如 `cat`, `less`, `grep`。而 `bash` 脚本中，读取文件通常用 `cat` 一次性读取; 或者 `read` 命令配合循环逐行读取。逐行读取允许你在读取过程中处理每一行数据，比如过滤或替换文本。

除了Bash内置命令，也有`awk`和`sed`等文本处理工具。它们功能强大，用于复杂文本处理。

每种方法都有其场景。快速查看文件内容用 `cat` 或 `less`。需要复杂处理时，`awk` 或 `sed`可能更合适。要注意的是，对于大文件，一次性读取可能会占用大量内存，逐行读取更有效。

## See Also (另请参阅)
- GNU Coreutils: https://www.gnu.org/software/coreutils/manual/coreutils.html
- Bash Reference Manual: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: http://www.tldp.org/LDP/abs/html/
