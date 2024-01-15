---
title:                "读取命令行参数"
html_title:           "Ruby: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为何会阅读命令行参数？

在编程中，我们经常需要通过命令行传递一些参数给程序，这样就可以在不改变源代码的情况下根据需要修改程序的行为。阅读命令行参数可以让我们更灵活地使用程序，提高代码的可复用性和可调试性。

## 如何阅读命令行参数

```Ruby
# 在Ruby中，我们可以使用ARGV数组来接收命令行参数
# 例如，在命令行输入：ruby program.rb hello world
# 则ARGV数组的元素为["hello", "world"]
# 我们可以按照传入的参数顺序来使用ARGV数组中的元素
puts "Hello #{ARGV[0]}!"  # 输出：Hello hello!
puts "Welcome to #{ARGV[1]}!"  # 输出： Welcome to world!
```

## 深入了解阅读命令行参数

除了使用ARGV数组来接收参数外，我们还可以使用标准库中的OptionParser类来更灵活地处理命令行参数。通过定义不同的选项和参数，我们可以实现更复杂的命令行操作。同时，也可以使用环境变量来设置默认值，使我们的程序更加灵活。

## 参考链接

- [Ruby官方文档 - Command Line Arguments](https://ruby-doc.org/core-2.7.0/doc/command_line_rdoc.html)
- [Ruby官方文档 - OptionParser类](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html)
- [Ruby on Rails入门教程 - 阅读命令行参数](https://railstutorial-china.org/ruby-on-rails-tutorial-v4.0.0/chapters/command-line/#preparing_arguments)