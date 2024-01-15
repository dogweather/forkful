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

#为什么

临时文件在Ruby编程中扮演着重要的角色，它们可以帮助我们处理复杂的数据和操作系统资源。通过创建临时文件，我们可以更轻松地处理临时数据，同时避免在程序结束后留下不必要的残留文件。

#如何使用

通过使用Ruby的内置库，我们可以轻松地创建临时文件。以下是一个基本的示例：

```ruby
tempfile = Tempfile.new('ruby-temp')
puts "临时文件的路径是： #{tempfile.path}"
tempfile.write("这是临时文件中的内容。")
tempfile.close
```
输出：
```
临时文件的路径是： /tmp/ruby-temp20180725-94075-opd6pm
```

我们可以查看临时文件的内容：

```ruby
puts File.read(tempfile.path)
```

输出：
```
这是临时文件中的内容。
```

当程序结束后，临时文件会自动被删除。

#深入了解

在创建临时文件时，我们可以指定文件名的前缀和后缀。默认的前缀是"ruby-temp"，后缀是系统时间戳和进程ID。我们也可以手动设置前缀和后缀，以便更容易识别和管理临时文件。

同时，我们还可以指定临时文件的文件夹路径。默认的临时文件夹是系统的 "/tmp" 目录，但也可以根据需要更改为其他目录。

除了创建临时文件，Ruby还提供了其他一些相关的方法，如创建临时目录和临时文件的操作。这些方法都可以在Ruby官方文档的Tempfile类中找到更详细的用法说明。

#另外阅读

- [Ruby官方文档 - Tempfile类](https://ruby-doc.org/stdlib-2.5.1/libdoc/tempfile/rdoc/Tempfile.html)
- [RubyMonk - 创建和管理临时文件](https://rubymonk.com/learning/books/4-ruby-primer-ascent/chapters/45-advanced-variables/lessons/105-creating-and-managing-temporary-files)