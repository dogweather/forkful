---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 是什么&为什么？

创建临时文件是编程过程中的一项常见任务，程序员可以在这些文件中存储临时数据或为某些运行时操作提供空间。这样做通常是为了节省内存，提高性能，同时防止对长期存储的数据造成意外损害。

## 怎么做:

在 Fish Shell 中创建临时文件的示例代码和输出如下:

```Fish Shell
# 创建临时文件
set tmpfile (mktemp)

# 显示临时文件路径
echo $tmpfile
```

输出:

```Fish Shell
/tmp/tmp.yZ5KCnQdh9
```

## 深入解析

创建临时文件的概念在计算年代早期就已经出现，目的是为了保留大量数据的临时存储空间，以减轻内存压力和提高性能。

交替方案包括直接在内存中存储数据（如使用数组或列表）或直接写入到长期存储的数据文件中。然而，这两种方法都有缺点。直接使用内存可能会占用大量资源，而直接写入常驻文件可能会导致数据丢失风险。

Fish Shell 的 `mktemp` 命令会在 `/tmp`目录下创建一个临时文件，该目录通常是一个独立的文件系统，具备很快的读写速度。 `mktemp` 能够随机生成一个独一无二的文件名，从而确保文件的唯一性。

## 另请参阅

- Fish Shell 文档: [http://fishshell.com/docs/current/index.html](http://fishshell.com/docs/current/index.html)
- `mktemp` 命令文档: [https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)
- UNIX 和 Linux 的临时文件和目录:  [http://www.tldp.org/LDP/Linux-Filesystem-Hierarchy/html/tmp.html](http://www.tldp.org/LDP/Linux-Filesystem-Hierarchy/html/tmp.html)