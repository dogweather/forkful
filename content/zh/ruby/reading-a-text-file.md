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

## 为什么

读取文本文件是编程中常见的任务，它可以帮助我们从文件中获取数据，并进行后续的处理。通过学习如何读取文本文件，我们可以更容易地处理大量的文本数据，从而提高我们的工作效率。

## 如何

首先，我们需要使用Ruby内置的File类来打开文本文件。下面是一个简单的例子：

```Ruby
file = File.open("sample.txt") # 打开名为sample.txt的文本文件
```

接下来，我们可以使用File对象的read方法来读取文本文件的内容，并存储在一个变量中：

```Ruby
text = file.read # 将文本文件的内容保存在text变量中
```

现在，我们可以对这个变量进行操作，比如打印出其中的内容：

```Ruby
puts text # 输出文本文件的内容
```

我们也可以使用File类的foreach方法来遍历文本文件的每一行，并对每一行进行操作。比如，我们可以在控制台上打印每一行的内容：

```Ruby
File.foreach("sample.txt") do |line|
    puts line # 打印每一行的内容
end
```

除了基本的读取操作，Ruby还提供了各种各样的方法来处理文本文件。比如，我们可以使用scan方法来搜索文本文件中的特定单词，并将它们存储在一个数组中：

```Ruby
words = text.scan(/Ruby/) # 搜索文本文件中的单词"Ruby"并存储在数组中
```

带有正则表达式的scan方法可以更灵活地搜索我们想要的内容。例如，我们可以搜索文本文件中所有包含数字的行，并将它们存储在一个数组中：

```Ruby
numbers = text.scan(/\d+/) # 搜索文本文件中的数字并存储在数组中
```

通过学习如何读取文本文件，我们可以更好地处理文本数据，并在编程中运用这些方法。

深入学习：

除了基本的File类，Ruby还提供了许多其他类来处理文件操作，比如IO类和CSV类。如果你想要进一步了解如何处理文本文件，可以阅读官方文档或者参考下面的链接。

## 参考链接

- [Ruby官方文档 - File类](https://ruby-doc.org/core-3.0.0/File.html)
- [Ruby官方文档 - IO类](https://ruby-doc.org/core-3.0.0/IO.html)
- [Ruby官方文档 - CSV类](https://ruby-doc.org/stdlib-3.0.0/libdoc/csv/rdoc/CSV.html)
- [Ruby教程 - 读取文本文件](https://www.rubyguides.com/ruby-tutorial/working-with-files/)
- [Ruby On Rails教程 - 处理文本文件](https://www.tutorialspoint.com/ruby-on-rails/rails-file-operations.htm) 

## 参考链接