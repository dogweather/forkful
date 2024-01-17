---
title:                "创建临时文件"
html_title:           "Bash: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么？为什么？

创建临时文件是一种程序员经常用到的技巧，它可以在不影响原文件的情况下进行一些临时的操作。程序员会这样做是为了避免破坏原始数据，同时也方便回滚到原始数据状态。

## 怎么做？

```
Bash mktemp命令是创建临时文件的首选方法。
```

```Bash
# 创建一个临时文件并写入内容
tmpfile=$(mktemp)
echo "这是一个临时文件的内容" > $tmpfile 
cat $tmpfile
```

输出：
```
这是一个临时文件的内容
```

## 深入了解

创建临时文件最早是在Unix系统中被广泛使用的技巧，它的原理是使用操作系统提供的临时目录来创建文件。除了mktemp命令外，还可以使用touch命令来创建临时文件，不过mktemp命令更加灵活，可以设置临时文件名的前缀、后缀、存放位置等。除了创建临时文件外，程序员也可以使用临时目录来存放临时数据，保证数据的安全性。

创建临时文件也有其他的替代方法，比如可以使用shell的变量（如RANDOM）来动态生成文件名，或者使用/dev/shm目录来创建内存中的临时文件，这样可以提高读写速度。

## 参考资料

- [Bash中的日常技巧：创建临时文件](https://linux.cn/article-7035-1.html)
- [Unix中创建临时文件的详细步骤](https://www.tecmint.com/create-temporary-files-in-unix-linux/)
- [Linux命令行临时文件使用指南](https://www.cyberciti.biz/faq/linux-create-temporary-file/)