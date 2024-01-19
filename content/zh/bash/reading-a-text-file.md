---
title:                "读取文本文件"
html_title:           "Kotlin: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 什么和为什么?
读取文本文件是从文本文件中检索、分析并使用数据的过程。程序员这么做是为了操作和分析其中的数据。

## 如何做:
你可以用Bash命令 `cat` 来读取文本文件。例如，示例输入和输出如下:

```Bash
$ cat textfile.txt
这是一个文本文件内容
```

对于更为复杂的情况，Bash提供了`while read line`循环结构来逐行读取一個文件。例如：

```Bash
$ while read line; do echo "$line"; done < textfile.txt
这是一个文本文件内容
```

## 深度解析:
历史上，Bash在Version 7 Unix（1979年）中首次提供了 `cat` 和 `read` 命令，使得读取文本文件变得简单和直接。其他方法，比如使用`awk`或`sed`，也可以用于读取和操作文本文件，但是它们的用法比较复杂。

关于读取文本文件的内部实现，Bash cmdline会调用C库函数fread()从磁盘中读取文件到内存，然后将该内存数据提供给用户使用。

## 另请参阅:
关于Bash指南，可以访问[GNU官方用户手册](https://www.gnu.org/software/bash/manual/bash.html)。

更多关于`cat`和`read`命令的详细信息，可以看看这个文章: [Linux `cat` command](https://www.howtoforge.com/linux-cat-command/) 和 [Linux `read` command](https://www.howtoforge.com/linux-read-command/)。

关于`awk`和`sed`，可以参考[LINUX AWK命令简介](https://www.jb51.net/article/44545.htm) 和 [SED命令教程](https://coolshell.cn/articles/9104.html)。