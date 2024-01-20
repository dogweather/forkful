---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 什么是命令行参数以及为什么我们要读取？ (What & Why?)

命令行参数是传递给脚本的信息，它帮助我们定制程序功能以适应不同的需求。从命令行读取参数能够使程序更加灵活且能够处理不同的使用情况。

## 这是如何做到的？ (How To:)

首先，让我们来看一个简单的实例。在你的脚本中，你可以使用全局`ARGV`数组来访问这些参数。

```Ruby
# greeting.rb
name = ARGV.first  
puts "Hello, #{name}!"
```

当你在命令行中运行这个程序时，如`ruby greeting.rb Alice`，结果将会是`Hello, Alice!`。

## 对此进行深度研究 (Deep Dive)

在深入学习如何从Ruby命令行读取参数之前，有三个主要的领域值得关注：历史背景，不同的方法，以及读取命令行参数的实际内部工作方式。

1. 历史背景: 早在计算机诞生的初期，命令行就被用作用户与计算机的交互接口。理解如何读取和使用命令行参数可以帮助我们编写更具适应性和灵活性的脚本。

2. 不同的方法: 尽管全局`ARGV`数组是读取命令行参数的最常见方法，也有一些标准库，如 `OptionParser`或`optparse`，它们提供了更详细的功能，可以处理选项以及选项后面的参数。

3. 实现细节: 当调用`ruby`命令并传入一个脚本时，脚本名后面的所有参数都会被视为命令行参数加入`ARGV`数组。脚本名存储在全局变量`$0`中。

## 参见 (See Also)

- Ruby 官方文档，ARGV: https://ruby-doc.org/core-2.7.0/ARGV.html
- OptionParser类的详细指南: https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html
- Stack Overflow, 关于Ruby命令行参数的讨论: https://stackoverflow.com/questions/1334555/ruby-command-line-arguments