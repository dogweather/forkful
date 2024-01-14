---
title:                "Ruby: 读取命令行参数"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么阅读命令行参数？

阅读命令行参数是一种非常方便的方法来指定程序在执行时所需的参数。通过使用命令行参数，您可以在运行程序时输入不同的值，而无需修改程序本身。这使得程序更加灵活和可重用，因为您可以使用不同的参数来测试不同的情况。

# 如何阅读命令行参数

要阅读命令行参数，您可以使用Ruby中的ARGV数组。这个数组包含了所有从命令行传递给程序的参数，您可以在程序中使用它们。

以下是一个简单的代码示例，展示如何使用ARGV数组来读取第一个参数并将其打印出来：

```Ruby
first_arg = ARGV[0]
puts "您输入的第一个参数是：#{first_arg}"
```

如果您在命令行输入`ruby program.rb hello`，那么上面的代码将打印出`您输入的第一个参数是：hello`。

# 深入研究

命令行参数可以根据您的程序的需要进行解析。您可以使用`ARGV`数组的方法，如`length`和`join`来检查传递给程序的参数的数量和内容。您还可以使用正则表达式来匹配特定的参数格式，并在程序中执行相应的逻辑。

除了使用ARGV数组，您还可以使用Ruby的OptionParser类来处理复杂的命令行选项。这个类可以帮助您定义可接受的选项和默认值，并将它们转换为易于使用的格式。

# 参考链接

- [Ruby - Command Line Arguments](https://www.rubyguides.com/2018/10/ruby-command-line-arguments/)
- [Ruby - ARGV Class](https://ruby-doc.org/core-2.7.1/ARGF.html)
- [Ruby - OptionParser Class](https://ruby-doc.org/stdlib-2.7.1/libdoc/optparse/rdoc/OptionParser.html)

# 参见

- [Ruby命令行参数入门指南](https://medium.com/@Lancer209a/ruby%E5%91%BD%E4%BB%A4%E8%A1%8C%E5%8F%82%E6%95%B0%E5%85%A5%E9%97%A8%E6%8C%87%E5%8D%97-e0468808c651)
- [Ruby中解析命令行选项的最佳实践](https://longliveseptember.com/ruby-command-line-options-best-practices-zh/)
- [命令行参数和选项处理的快速指南](https://blog.ghostztianzhu.com/ruby-command-line-args-and-opt-parse-quick-and-dirty-zh/)