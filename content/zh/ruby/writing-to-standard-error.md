---
title:                "写标准错误"
html_title:           "Ruby: 写标准错误"
simple_title:         "写标准错误"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 什么是标准错误输出？为什么程序员要使用它？

标准错误输出是一种编程概念，它允许程序员在代码中指定错误消息的目的地，而不是默认的标准输出。这对于调试和错误处理非常有用，因为它可以将错误消息与其他打印信息区分开来。程序员使用标准错误输出来向他们提供有用的信息，以便更轻松地解决错误。

## 如何使用标准错误输出：

```Ruby
puts "This is standard output" # 正常输出语句
$stderr.puts "This is standard error output" # 标准错误输出语句
```

这里我们在标准输出和标准错误输出中都打印了一条消息，但是输出结果会有所不同。标准输出的消息会显示在控制台中，而标准错误输出的消息会以红色显示，并且有一个前缀为"error"的提示符。

## 深入探讨：

标准错误输出最常见的使用情况是在调试代码时。通过将错误消息输出到标准错误，程序员可以将它与标准输出区分开来，以便更容易发现问题所在。此外，还有一些替代方法来记录错误信息，比如日志记录系统。然而，使用标准错误输出仍然是一种简单有效的方式来处理错误。

## 参考资料：

了解更多关于标准错误输出的用法，请参考以下链接：

- [标准错误输出 - Ruby官方文档](https://ruby-doc.org/core-3.0.0/IO.html#method-i-puts)
- [标准错误输出 - Ruby快速教程](https://www.rubyguides.com/2019/02/ruby-stderr-stdout/)