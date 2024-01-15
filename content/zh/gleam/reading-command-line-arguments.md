---
title:                "读取命令行参数"
html_title:           "Gleam: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么读取命令行参数？

如果你是一名Gleam编程语言的入门者，或者想要学习如何从命令行获取输入，那么阅读命令行参数是必不可少的。通过阅读本文，你将学会如何在Gleam中灵活地读取命令行参数，以及为什么要这样做。

## 如何读取命令行参数？

读取命令行参数在Gleam中非常简单，只需要使用标准库中的`gleam/args`模块。让我们来看一个例子，假设我们要从命令行获取一个名字并输出它：

```
import gleam/args

fn main() {
  let args = args.arguments()
  let name = case args {
    [] -> "John"
    [name] -> name
    _ -> "Invalid input"
  }
  io.println("Hello, #{name}!")
}
```

我们使用`args.arguments()`函数来获取命令行参数，并使用`case`表达式来匹配不同的参数情况。上面的代码的基本思路是：如果命令行参数为空，则默认输出"John"；如果只有一个参数，则输出该参数作为名字；否则，输出"Invalid input"。

让我们来测试一下：

```
$ gleam run hello.gleam
Hello, John!

$ gleam run hello.gleam Alice
Hello, Alice!

$ gleam run hello.gleam Alice Bob
Hello, Invalid input!
```

通过这个例子，我们可以看到如何使用`args`模块来读取命令行参数，并使用`case`表达式进行匹配。

## 深入了解

现在，让我们深入了解一下如何读取命令行参数。在Gleam中，命令行参数是以字符串列表的形式存在的，通过使用`args.arguments()`函数，我们可以获取这个字符串列表。然后，我们可以使用`case`表达式对这个列表进行匹配，来做出不同的处理。

除了`args`模块，我们也可以从[Gleam文档](https://gleam.run/documentation/standard-library/args/)中学习更多相关的标准库函数，例如`args.ordered`和`args.named`函数，它们可以帮助我们更灵活地读取命令行参数。

# 参考链接

- [Gleam文档 - 读取命令行参数](https://gleam.run/documentation/standard-library/args/)
- [Gleam仓库 - args模块源代码](https://github.com/gleam-lang/gleam_stdlib/blob/master/src/gleam/args.erl)