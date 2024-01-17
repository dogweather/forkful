---
title:                "书写文本文件"
html_title:           "Bash: 书写文本文件"
simple_title:         "书写文本文件"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

写文本文件指的是使用文本编辑器或编程语言将文字内容保存到文件中。程序员常常使用文本文件来储存配置信息、日志记录或其他重要的数据。这样做可以让信息更加有条理并且容易读取。

## 如何：

使用Bash编程语言可以很容易地写入文本文件。下面是一个例子：

```Bash
# 创建一个名为test.txt的文本文件
touch test.txt
# 向文件中写入一行内容
echo "Hello world!" > test.txt
# 显示文件的内容
cat test.txt

# 输出：
Hello world!
```

## 深入探讨：

编写文本文件已经有很长的历史了。早期的计算机并不具备图形界面，所以程序员必须通过编写文本文件来创建程序和储存数据。现在，我们有了许多其他的选择，比如使用数据库等。但是对于简单的文本文档来说，使用文本文件仍然是一个便捷的选择。

## 参考：

- [Bash参考手册](https://www.gnu.org/software/bash/manual/bash.html)
- [如何使用Bash来管理文件和目录](https://www.digitalocean.com/community/tutorials/how-to-manage-files-from-the-linux-command-line)