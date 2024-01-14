---
title:                "Ruby: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

阅读文本文件在编程中是非常常见的任务。它可以帮助我们处理大量的数据，例如CSV文件，或者从网站上爬取的信息。阅读文本文件也是学习Ruby编程的基础之一。

## 如何做

最简单的方法是使用Ruby的File类来读取文件。首先，我们需要使用`open`方法打开文件，并指定文件名和打开模式。接下来，我们可以使用`read`方法来读取整个文件内容，或者使用`each`方法来逐行读取文件内容。最后，我们可以使用`close`方法来关闭文件。

```Ruby
file = File.open("sample.txt", "r")
puts file.read
file.close

file.each do |line|
  puts line
end
file.close
```

上面的代码演示了两种不同的读取文件的方法。`read`方法会将整个文件的内容作为一个字符串返回，而`each`方法会逐行读取文件，并将每一行作为一个字符串返回。

## 深入了解

除了基本的读取文件操作，Ruby还提供了许多其他有用的方法来处理文件。例如，我们可以使用`gets`方法来逐行读取文件内容，并使用`split`方法来分隔每一行的内容。我们也可以使用正则表达式来对文件内容进行匹配和替换等操作。

另外，我们还可以使用`write`方法来写入文件内容，以及创建和删除文件等操作。深入了解这些方法可以让我们更加灵活地处理文本文件。

## 参考链接

- [Ruby文档：File类](https://ruby-doc.org/core-3.0.2/File.html)
- [Ruby教程：文件和文件操作](https://www.runoob.com/ruby/ruby-file-io.html)
- [Ruby基础教程第11章：文件操作](https://www.liaoxuefeng.com/wiki/896043488029600/897013441037824)

## 参考资料

- [Markdown语法指南](https://www.markdownguide.org/basic-syntax/)
- [Markdown中文简明教程](https://www.jianshu.com/p/7bd23251da0a)