---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在编程中，字符串连接是指将两个或多个字符串组合在一起。程序员进行字符串连接以便处理用户输入，生成报告，构建动态SQL查询等等。

## 如何操作：

在Elixir中，我们可以使用`<>`运算符来连接字符串。

```Elixir
IO.puts("Hello" <> " World!")
```

运行这段代码，将在控制台输出：

```
Hello World!
```

另外，还可以使用`String.concat/2`函数组合字符串：

```Elixir
IO.puts(String.concat("Hello", " World!"))
```

输出将是：

```
Hello World!
```

## 深入探讨

- **历史背景**：Elixir语言是在2011年由 José Valim创建，目的是改进Erlang语言的某些方面，包括字符串连接。

- **替代方案**：在Elixir中，除了使用 `<>` 运算符和 `String.concat/2` 函数外，也可以使用 `String.join/1` 和 `String.join/2` 函数来连接字符串。

- **实现细节**：在内部，`<>` 是由 `Kernel.def` 函数定义的。`String.concat/2` 也是如此。当程序使用这些函数时，它们会调用底层的Erlang函数以提高性能。

## 参考资料

如果你想更深入了解Elixir时，下面是一些可能会有用的资源：

- [Elixir Guide](https://elixir-lang.org/getting-started/introduction.html)
- [Elixirschool](https://elixirschool.com/)
- [Erlang and Elixir Docs](https://erlang.org/doc/)
- [The Pragmatic Studio - Elixir](https://pragmaticstudio.com/elixir)