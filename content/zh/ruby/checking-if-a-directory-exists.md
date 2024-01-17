---
title:                "检查目录是否存在"
html_title:           "Ruby: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 什么&为什么？

在编程中，我们经常需要检查一个目录是否存在。这有助于我们避免在代码中引用不存在的目录，从而防止程序出错。检查目录是否存在的另一个常见原因是为了确保我们需要的文件和资源都存在，并且程序可以顺利运行。

# 如何：

```Ruby
# 使用File类的‘exists?’方法来检查目录是否存在，若存在则返回true，反之则返回false。
File.exists?("my_directory")  # => true/false
```

# 深入探究：

## 历史背景：

在过去，程序员可能会使用操作系统或系统调用来检查目录是否存在。但是，Ruby的File类提供了更简便的方法，让我们可以直接在代码中使用。此外，现在的操作系统也提供了更好的目录管理功能，使得检查目录是否存在更容易。

## 替代方法：

除了File类的‘exists?’方法，我们也可以使用‘directory?’和‘readable?’方法来检查目录是否存在。‘directory?’方法可以确保目录的类型是一个目录，而‘readable?’方法则可以检查目录是否具有读取权限。

## 实现细节：

Ruby的‘Dir’和‘File’类都是通过调用操作系统提供的系统调用来实现检查目录是否存在的功能。这些类还提供了更多的方法来操作文件和目录，如创建、删除、移动和重命名等。

# 看看这些：

- [File类文档](https://ruby-doc.org/core-3.0.2/File.html)
- [Dir类文档](https://ruby-doc.org/core-3.0.2/Dir.html)