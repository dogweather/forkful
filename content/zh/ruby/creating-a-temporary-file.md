---
title:                "Ruby: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，有时我们需要暂时存储一些数据，但又不想将其永久保存。这时，使用临时文件是一个很好的选择。临时文件能够在我们不需要它们时自动被删除，从而节省存储空间。

## 如何

创建临时文件非常简单，我们只需要使用Ruby的内置函数来实现。以下是一个小例子，演示了如何创建一个名为“temp.txt”的临时文件，并向其中写入一些数据。

```Ruby
require 'tempfile'

temp_file = Tempfile.new('temp.txt') #创建一个名为temp.txt的临时文件
temp_file.write("这是一个临时文件") #向临时文件中写入数据
puts temp_file.path #输出临时文件的路径，可以发现它在系统默认的临时文件夹中
```

运行以上代码，我们可以看到输出的路径类似于“/var/folders/q1/zn209m595d71s6h42rlbkdj40000gn/T/temp20190319-7388-3qk99r.txt”。

## 深入探讨

临时文件的创建实际上是利用了操作系统的机制。在创建临时文件时，Ruby会首先在操作系统的临时文件夹中创建一个随机的文件名，然后再在其中写入数据。在程序执行完毕后，临时文件会被自动删除。

当然，我们也可以手动删除临时文件，只需要调用`temp_file.unlink`即可。此外，我们还可以指定临时文件的存储路径，或者在创建临时文件时指定其打开模式、权限等。

## 参考链接

- [Ruby标准库文档 - Tempfile](https://ruby-doc.org/stdlib-2.6.1/libdoc/tempfile/rdoc/Tempfile.html)
- [《Ruby基础教程》- 临时文件操作](http://www.w3cschool.cn/ruby/ruby-temporary-file.html)
- [《Ruby编程语言》- 使用Tempfile类生成临时文件](https://ruby-china.org/topics/992)