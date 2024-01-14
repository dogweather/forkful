---
title:                "Bash: 创建临时文件。"
simple_title:         "创建临时文件。"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 为什么

临时文件在编程中扮演着非常重要的角色。它们可以帮助我们存储和处理临时数据，而不会占用过多的内存空间。临时文件还可以帮助我们在程序运行过程中保留一些重要的信息，以便于后续使用。因此，创建临时文件是一项非常有用的编程技巧，可以帮助我们更有效地编写程序。

# 如何做

```Bash
# 创建一个临时文件
temp_file=$(mktemp)

# 将数据写入临时文件
echo "这是一个临时文件" > $temp_file

# 从临时文件读取数据
cat $temp_file

# 删除临时文件
rm $temp_file
```

输出：

这是一个临时文件

# 深入了解

在Bash中，我们可以使用mktemp命令来创建临时文件。使用这个命令，我们可以生成一个唯一的文件名，并将其赋值给一个变量。然后，我们可以使用这个变量来操作临时文件，比如将数据写入文件或从文件中读取数据。最后，我们需要在程序结束时手动删除临时文件，以确保不会耗费存储空间。

# 参考链接

- [Bash文档](https://www.gnu.org/software/bash/manual/bash.html)
- [Linux命令mktemp用法详解](https://blog.51cto.com/nickge/1897287)
- [编程中十分常用的几个Linux命令](https://blog.csdn.net/u012067966/article/details/52037972)

# 参看

- [Markdown语法指南]（http://www.markdown.cn/）
- [Bash编程的基本概念]（https://www.ibm.com/developerworks/cn/linux/l-bash-components）