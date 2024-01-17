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

## 这是什么 & 为什么？

读取命令行参数是程序员经常做的一件事情，它允许他们在运行程序时通过命令行输入一些参数。这样就可以改变程序的行为，让它更具有灵活性。

## 如何：

```Ruby
# 这是一个简单的示例，演示如何读取一个命令行参数并将其打印出来。

# 首先，我们需要导入 Ruby 内置的 ARGV 模块，它可以帮助我们读取命令行参数。
require 'ARGV'

# 下一步是定义一个数组，用于存储我们读取到的命令行参数。
args = []

# 然后，我们可以使用 each 方法来遍历 ARGV 数组，并将每个参数添加到我们定义的 args 数组里面。
ARGV.each do |arg|
  args << arg
end

# 最后，我们可以使用 puts 方法将 args 数组里面的参数打印出来。
puts "命令行参数为：#{args}"
```

输出为：命令行参数为：[参数1, 参数2, 参数3]

## 深入了解：

读取命令行参数这个功能在很多编程语言中都有，它为程序员提供了一种方便的方式来交互式地控制程序的行为。除了使用 ARGV 模块，还有一种常用的方式是使用环境变量来读取命令行参数。另外，如果程序需要读取复杂的命令行参数，还可以使用 OptionParser 类来帮助处理。不管采用哪种方式，读取命令行参数都是让程序更加灵活实用的一种方法。

## 参考链接：

- [Ruby 官方文档 - ARGV 模块](https://ruby-doc.org/core-3.0.2/ARGV.html)
- [Ruby 官方文档 - OptionParser 类](https://ruby-doc.org/stdlib-3.0.2/libdoc/optparse/rdoc/OptionParser.html)
- [Ruby 之家 - Ruby 命令行参数介绍](https://www.ruby-cn.org/articles/commandlineargs/)