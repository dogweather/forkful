---
title:                "标准错误的写作"
html_title:           "Elixir: 标准错误的写作"
simple_title:         "标准错误的写作"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

作为一名程序员，我们经常需要与错误信息打交道。在日常工作中，写入标准错误流（standard error）可以方便我们调试和诊断错误。同时，它也可以帮助我们更好地定位问题，提高代码的健壮性和可靠性。

## 如何进行

如果我们想要在Elixir中写入标准错误流，我们可以使用`IO.puts/1`函数。它接受一个字符串作为参数，并将其写入标准错误流。下面是一个简单的例子：

```Elixir
IO.puts("This is an error message.") 
```
运行以上代码，我们将看到错误信息被输出到控制台，而不是标准输出流。下面是输出结果的截图：

![Error message output](/images/elixir-article-error-output.png)

当然，在实际开发中，我们通常会根据具体的场景来写入标准错误流。例如，在一个函数中，当参数不符合预期时，我们可以使用`IO.puts/1`来打印错误信息并结束函数的执行。

## 深入了解

在Elixir中，`IO.puts/1`实际上是调用`IO.puts/2`函数的简写形式。`IO.puts/2`函数的第二个参数是一个可选的IO设备，我们可以指定为`:stderr`来将输出发送到标准错误流。同时，我们也可以使用`IO.puts/2`函数来打印更复杂的数据结构，如列表、元组、Map等。下面是一个使用`IO.puts/2`函数的例子：

```Elixir
IO.puts(:stderr, ["This", "is", "an", "error", "message."])
```

除了`IO.puts/2`函数外，Elixir还提供了很多其他的IO函数来帮助我们写入不同的IO流，如`IO.write/2`、`IO.puts/3`等。有兴趣的读者可以查看官方文档来了解更多。

## 参考链接

- Elixir官方文档：https://hexdocs.pm/elixir/IO.html
- Elixir School中文版：https://elixirschool.com/zh-cn/