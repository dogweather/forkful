---
title:    "Gleam: 读取命令行参数"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## 为什么

阅读命令行参数是编程中的常见任务，它允许我们以不同的方式使用相同的代码，使程序更加灵活和通用。了解如何读取命令行参数可以帮助开发人员轻松地调整程序的行为，使其适用于不同的情况。

## 如何做

在Gleam中，要读取命令行参数非常简单。首先，在您的代码中导入Gleam的`command`模块，然后使用`command.read()`函数来读取参数。让我们来看一个例子：

```
Gleam importio
import command
fn main() {
  args = command.read()
  io.println("Hello, " ++ args[0] ++ "!")
}
```

在这个例子中，我们导入了Gleam的`importio`模块，然后使用`command.read()`读取命令行参数，将其存储在`args`变量中。接下来，我们使用`io.println()`函数打印出`args`中的第一个参数，也就是我们在命令行中传入的第一个参数。假设我们在命令行中传入`"World"`作为参数，那么上面的代码将打印出`"Hello, World!"`。

## 深入探讨

除了通过`command.read()`来读取所有的命令行参数外，您还可以使用`command.read_name()`来读取指定的参数。例如，假设我们只想读取第二个参数，我们可以使用`command.read_name(1)`来读取它。此外，您还可以使用`command.has_name()`来检查是否存在指定的命令行参数。

## 参考链接

- Gleam官方文档：https://gleam.run/
- Gleam Github仓库：https://github.com/gleam-lang/gleam
- 关于命令行参数更多的信息：https://en.wikipedia.org/wiki/Command-line_interface