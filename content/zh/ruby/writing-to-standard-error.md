---
title:                "写入标准错误"
html_title:           "Ruby: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么要写入标准错误流

编写程序时，我们经常会遇到各种错误。为了更有效地调试代码并找出问题所在，我们会将错误信息写入标准错误流。这样，我们可以更快地找到问题并解决它们。

## 如何编写到标准错误流

要将错误信息写入标准错误流，我们可以使用Ruby的 `warn` 方法。例如，当我们想要打印出一个错误信息时，可以这样写：

```Ruby
warn "这是一个错误信息"
```

当我们运行上面的代码时，输出将会是：

```Ruby
这是一个错误信息
```

## 深入了解标准错误流

标准错误流是一个指向 `STDERR` 对象的指针，它是一个全局变量。这意味着我们可以在程序的任何地方使用 `STDERR` 对象来写入错误信息。此外，我们也可以使用 `raise` 方法来抛出异常，将错误信息写入标准错误流。所有这些都可以帮助我们更有效地排查程序中的错误。

## 参考链接

- [Ruby文档：标准错误流](https://ruby-doc.org/core-#{RUBY_VERSION}/IO.html#class-IO-label-Standard+Streams)
- [RubyMonk：处理错误](https://rubymonk.com/learning/books/1-ruby-primer/chapters/18-error-handling/lessons/64-catch-and-throw)