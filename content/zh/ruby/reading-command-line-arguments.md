---
title:    "Ruby: 读取命令行参数"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么？

在编程中，我们经常需要从终端运行程序并传递一些参数。通过阅读命令行参数，我们可以轻松地修改程序的行为，从而节省时间和精力，并更有效地完成任务。因此，学习如何读取命令行参数是非常重要的。

## 如何？

首先，让我们看一个简单的例子。假设我们有一个Ruby程序，现在我们想要从命令行传入两个参数，分别是姓名和年龄。我们可以这样写：

```ruby
name = ARGV[0]
age = ARGV[1]
puts "你好，#{name}。您的年龄是#{age}岁。"
```

在终端中运行程序时，我们需要在程序文件名后添加参数，如下所示：

```bash
ruby hello.rb John 25
```

运行结果将是：

```
你好，John。您的年龄是25岁。
```

在这个例子中，我们使用了一个内置的Ruby变量ARGV来读取命令行参数。这个变量是一个数组，存储了所有传入的参数。通过指定索引，我们可以轻松地获取所需的参数。

除了使用ARGV变量之外，还有其他的方法来读取命令行参数。例如，我们可以使用OptionParser库来定义一个更复杂的参数列表，并获取用户输入的值。这里有一些参考链接供您学习更多。

## 深入探讨

阅读命令行参数的能力是一项非常有用的技能，它可以帮助我们更好地控制和优化我们的程序。一旦我们掌握了基本的方法，我们就可以找到更多的方法来利用命令行参数，并根据我们的需求进行调整。同时，学习如何处理命令行参数也是成为一名优秀软件工程师的基本技能之一。

## 参考资料

- [ruby-doc.org: Command Line Arguments](https://ruby-doc.org/core-2.6.3/ARGF.html)
- [Ruby Guides: Command Line Arguments](https://www.rubyguides.com/2018/10/ruby-command-line-arguments/)
- [OptionParser Library](https://ruby-doc.org/stdlib-2.6.3/libdoc/optparse/rdoc/OptionParser.html)

## 同时查看

- [命令行选项的参数处理方法](https://www.cnblogs.com/stephen-liu74/archive/2011/10/07/2236715.html)