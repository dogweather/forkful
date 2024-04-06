---
date: 2024-01-20 17:53:59.734228-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5386\u53F2\u4E0A\uFF0C\u6587\
  \u672C\u6587\u4EF6\u59CB\u7EC8\u662F\u4FE1\u606F\u4EA4\u6362\u7684\u57FA\u7840\u3002\
  Unix\u54F2\u5B66\u5F3A\u8C03\u4F7F\u7528\u7B80\u5355\u7684\u3001\u6587\u672C\u5F62\
  \u5F0F\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\u3002Bash \u501F\u6B64\u4F20\u7EDF\
  \uFF0C\u5185\u7F6E\u4E86\u8BB8\u591A\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u7684\u5DE5\
  \u5177\uFF0C\u5982 `cat`, `less`, `grep`\u3002\u800C `bash` \u811A\u672C\u4E2D\uFF0C\
  \u8BFB\u53D6\u6587\u4EF6\u901A\u5E38\u7528 `cat` \u4E00\u6B21\u6027\u8BFB\u53D6\
  ; \u6216\u8005 `read`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.190524-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5386\u53F2\u4E0A\uFF0C\u6587\u672C\u6587\
  \u4EF6\u59CB\u7EC8\u662F\u4FE1\u606F\u4EA4\u6362\u7684\u57FA\u7840\u3002Unix\u54F2\
  \u5B66\u5F3A\u8C03\u4F7F\u7528\u7B80\u5355\u7684\u3001\u6587\u672C\u5F62\u5F0F\u7684\
  \u6570\u636E\u4EA4\u6362\u683C\u5F0F\u3002Bash \u501F\u6B64\u4F20\u7EDF\uFF0C\u5185\
  \u7F6E\u4E86\u8BB8\u591A\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u7684\u5DE5\u5177\uFF0C\
  \u5982 `cat`, `less`, `grep`\u3002\u800C `bash` \u811A\u672C\u4E2D\uFF0C\u8BFB\u53D6\
  \u6587\u4EF6\u901A\u5E38\u7528 `cat` \u4E00\u6B21\u6027\u8BFB\u53D6; \u6216\u8005\
  \ `read` \u547D\u4EE4\u914D\u5408\u5FAA\u73AF\u9010\u884C\u8BFB\u53D6\u3002\u9010\
  \u884C\u8BFB\u53D6\u5141\u8BB8\u4F60\u5728\u8BFB\u53D6\u8FC7\u7A0B\u4E2D\u5904\u7406\
  \u6BCF\u4E00\u884C\u6570\u636E\uFF0C\u6BD4\u5982\u8FC7\u6EE4\u6216\u66FF\u6362\u6587\
  \u672C\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

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
