---
title:                "读取文本文件"
html_title:           "Ruby: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 什么是文本文件 & 为什么程序员会用它
读取文本文件通常意味着从一个文本文件中读取内容并在程序中使用它。程序员这样做的原因是为了获取文件中的数据和信息，然后在程序中处理、展示或存储这些数据。

## 如何实现
```Ruby
# 打开并读取文本文件
file = File.open("text_file.txt", "r")
contents = file.read
file.close

# 将读取的内容打印出来
puts contents 
```
输出：
```
这是一个文本文件。
它有很多有用的信息。
```

## 深入了解
读取文本文件已经成为现代编程的必不可少的部分。它可以用来从外部文件中获取数据，而不必编写硬编码的数据集合。此外，Ruby也提供了其他方法来读取文件内容，如`IO.readlines`和`File.readline`。

## 参考资料
了解更多有关读取文本文件的信息，请参阅Ruby文档：https://ruby-doc.org/core-3.0.2/File.html#method-c-read。