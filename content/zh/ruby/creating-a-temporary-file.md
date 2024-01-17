---
title:                "创建临时文件"
html_title:           "Ruby: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么是临时文件？为什么程序员要创建它？

临时文件是程序员在运行程序时临时创建的文件。它们通常用于存储临时数据，比如在程序运行过程中产生的中间数据。创建临时文件可以帮助程序更高效地处理数据，同时也可以防止对正式文件的意外修改。

## 如何创建临时文件？

在Ruby中，我们可以使用`Tempfile`类来创建临时文件。首先，需要在程序中引入`tempfile`模块，然后使用`Tempfile.new`方法来创建临时文件对象。这个方法会在当前目录下创建一个唯一命名的临时文件，并返回对应的对象。我们可以通过调用对象的方法来向临时文件中写入数据，最后记得要关闭临时文件对象。

```Ruby
require 'tempfile'

# 创建临时文件
tempfile = Tempfile.new

# 写入数据到临时文件
tempfile.write("Hello World!")

# 关闭临时文件
tempfile.close

# 查看临时文件内容
puts File.read(tempfile.path)

# 输出： Hello World!
```

##深入探讨

创建临时文件的概念最早出现在操作系统层面，旨在为程序提供临时存储空间。在早期的Unix系统中，`/tmp`目录就是为此而设立的。而对于Ruby程序员来说，创建临时文件可以使用`File.open`方法，但`Tempfile`类提供了更加方便易用的接口。值得注意的是，在程序结束后，临时文件会自动被删除。

## 相关资源

- Ruby官方文档：https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html
- 关于临时文件的更多知识：https://en.wikipedia.org/wiki/Temporary_file