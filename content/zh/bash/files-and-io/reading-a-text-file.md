---
title:                "阅读文本文件"
aliases:
- /zh/bash/reading-a-text-file.md
date:                  2024-01-20T17:53:59.734228-07:00
model:                 gpt-4-1106-preview
simple_title:         "阅读文本文件"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/reading-a-text-file.md"
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
