---
title:                "Bash: 读取文本文件"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

阅读文本文件是编程中一项常用的技能，因为它可以帮助我们处理大量的文本数据。无论是分析日志文件、处理配置文件，还是从网页中提取信息，都需要读取文本文件。通过学习如何读取文本文件，你可以更有效地处理和利用文本数据，提高编程技能。

## 怎么做

要读取文本文件，首先需要打开文件。在Bash中，我们可以使用`cat`命令来打开文本文件。例如，`cat sample.txt`将会打开名为"sample.txt"的文本文件，并在终端显示文件中的内容。如果你想将文本文件的内容输出到另一个文件中，可以使用重定向符号`>`，例如`cat sample.txt > output.txt`将会把"sample.txt"中的内容复制到名为"output.txt"的文件中。

要逐行读取文本文件中的内容，可以使用`while`循环和`read`命令。示例如下：

```bash
while read line
do
  echo "$line"
done < sample.txt
```

以上代码将会逐行读取"sample.txt"中的内容，并将每行内容打印在终端上。你也可以使用`grep`命令来搜索文本文件中的特定内容。例如，`grep "keyword" sample.txt`将会输出所有包含"keyword"的行。

## 深入探讨

除了常见的`cat`、`while`和`grep`命令外，Bash还提供了许多其他命令和技巧来读取文本文件。例如，`head`命令可以显示文本文件的前几行内容，而`tail`命令可以显示文本文件的最后几行内容。如果你需要按照某个特定的字段来排序文本文件中的内容，可以使用`sort`命令。

Bash还提供了许多内置的变量来处理文本文件。例如，`$LINENO`变量可以获取当前行号，`$RANDOM`变量可以生成随机数。这些变量可以帮助你更方便地处理文本文件中的数据。

## 参考链接

- [Bash文本文件操作](https://www.redhat.com/sysadmin/parsing-csv-file-bash)
- [Bash手册](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)
- [Linux命令大全](https://linuxcommand.org/)