---
title:    "Clojure: 编写文本文件"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

为什么要写一个文本文件？作为一个Clojure程序员，你可能经常遇到需要存储数据或者在程序中导入外部数据的情况。一个文本文件是最简单、最基础的数据存储格式，使用Clojure可以简单地创建和读取这样的文件。

## 如何实现

要在Clojure中创建一个文本文件，我们可以使用`spit`函数，它接受文件名和要写入的数据两个参数。让我们先来创建一个空文本文件：

```Clojure
(spit "test.txt" "")
```

我们也可以向文本文件中写入内容，比如一个字符串：

```Clojure
(spit "test.txt" "这是一个测试文件。")
```

接下来，我们可以使用`slurp`函数来读取文本文件中的内容：

```Clojure
(slurp "test.txt") ; => "这是一个测试文件。"
```

## 深入了解文本文件

在Clojure中，我们可以使用字符串作为文本文件的主要数据类型。但是，也有一些库可以帮助我们更高效地处理和分析文本文件，比如[enlive](https://github.com/cgrand/enlive)和[clj-pdf](https://github.com/clj-pdf/clj-pdf)。

同时，我们也可以使用Clojure的文件操作函数，比如`file-seq`和`walk`来遍历文件系统中的文本文件。

## 参考资料

- [Clojure文档](https://clojure.org/)
- [Clojure Cookbook - Writing Data to Files](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/03_local-io/3-06_data-to-files.md)
- [How to Create and Read a Text File in Clojure](https://www.dummies.com/programming/how-to-create-and-read-a-text-file-in-clojure/)

## 参看

- [(原文档) https://your-clojure-cookbook.com/basic-filesystem/read-write-strings/write-to-file/](https://your-clojure-cookbook.com/basic-filesystem/read-write-strings/write-to-file/)