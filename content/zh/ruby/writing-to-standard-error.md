---
title:                "Ruby: 写入标准错误"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

编程有很多种方式来输出信息，其中一种是将信息打印到标准错误（standard error）流中。这种方法有时候也被称为“打印到屏幕底部”（printing to the bottom of the screen）或者“打印到控制台”（printing to the console），它可以通过代码中的`$stderr.puts`来实现。为什么要使用这种方法呢？现在就和我一起来探讨一下吧！

## 如何使用

首先，让我们来看一个简单的例子来使用`$stderr.puts`。假设我们有一个名为`hello.rb`的Ruby程序，它的内容如下：

```Ruby
puts "Hello, world!"
$stderr.puts "This is an error message."
```

运行这个程序，我们会得到以下输出：

```
Hello, world!
This is an error message.
```

可以看到，`$stderr.puts`打印出的内容显示在了`puts`打印的内容的下面，而且字体颜色也有所不同。这样做有什么好处呢？首先，这能够让我们区分出来哪些信息是正常输出，哪些是错误信息，这样在调试程序时会更加方便。其次，这样的输出也可以帮助我们更快地定位到错误的位置，从而更快地解决问题。

此外，我们还可以使用更多的`$stderr`方法来输出不同类型的错误信息，例如`$stderr.print`和`$stderr.write`。这些方法的具体用法可以根据你的需要来自行搜索。

## 深入了解

要理解为什么会有标准错误流这一概念，就需要了解一下程序是如何运行的。当我们运行一个程序时，它会创建三个标准流，分别是标准输入（standard input）、标准输出（standard output）和标准错误（standard error）。标准输入通常指的是键盘输入，标准输出通常指的是屏幕输出，而标准错误则通常指的是程序的错误输出。

当程序发生错误时，我们希望能够将这些错误信息打印出来，而不是让它们留在程序内部。这就是标准错误流的作用，它将错误信息输出到屏幕，从而帮助我们更快地发现和解决问题。

## 参考资料

- [Ruby Doc: Standard Error](https://ruby-doc.org/core-2.7.1/IOError.html#method-i-write)
- [Understanding Standard Streams in Ruby](https://www.rubyguides.com/2015/07/ruby-standard-error/)
- [Flushing Standard Output and Error Streams in Ruby](https://dev.to/nabeelvalapra/flushing-standard-output-and-error-streams-in-ruby-307p)
- [Master the Ruby Standard Streams](https://www.rubyguides.com/2015/07/ruby-standard-streams/)

## 参见

- [Ruby Doc: Standard Error](https://ruby-doc.org/core-2.7.1/IOError.html#method-i-write)
- [Ruby Doc: puts](https://ruby-doc.org/core-2.4.0/IO.html#method-i-puts)
- [Ruby Doc: print](https://ruby-doc.org/core-2.4.0/IO.html#method-i-print)
- [Ruby Doc: write](https://ruby-doc.org/core-2.4.0/IO.html#method-i-write)