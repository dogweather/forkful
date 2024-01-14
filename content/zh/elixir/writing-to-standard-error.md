---
title:                "Elixir: 标准错误输出编程"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Elixir 编程博客：如何利用标准错误输出

## 为什么？
使用标准错误输出可以帮助开发人员诊断和调试代码中的错误。当代码遇到错误时，标准错误输出可以将错误信息打印到控制台，从而帮助开发人员快速发现并解决问题。因此，写入标准错误输出是一种重要的程序员技能。

## 如何做？
看下面的示例代码，在Elixir中如何将内容写入标准错误输出：

```Elixir
IO.puts(:stderr, "这是一个标准错误输出示例")
```

运行上述代码后，你将在控制台上看到以下输出：

```Elixir
这是一个标准错误输出示例
```

如上所示，使用 `IO.puts` 函数并指定 `:stderr` 值来将内容写入标准错误输出。同样，也可以使用 `IO.inspect` 函数在调试时打印变量值到标准错误输出。

## 深入探讨
除了打印简单的字符串，Elixir 还提供了 `Kernel.inspect/2` 函数来格式化输出。参考下面的示例代码：

```Elixir
customer = %{"name" => "张三", "age" => 28}
IO.puts(:stderr, "客户信息是： #{inspect customer}")
```

运行上述代码后，你将在控制台上看到以下输出：

```Elixir
客户信息是： %{"age" => 28, "name" => "张三"}
```
如上所示，使用 `Kernel.inspect/2` 函数可以打印出更复杂的数据结构，如 map 和列表。

## 查看也可以
想要了解更多关于 Elixir 中标准错误输出的内容，请阅读官方文档。以下是一些相关的链接： 

- 官方文档：https://hexdocs.pm/elixir/IO.html#print/2 
- Elixir 教程：https://elixir-lang.org/getting-started/io-and-the-file-system.html 
- Elixir 论坛：https://elixirforum.com/t/how-to-write-to-stdout-or-stderr/383

 感谢阅读，希望本文对你有帮助！