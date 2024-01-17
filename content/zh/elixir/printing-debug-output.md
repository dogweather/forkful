---
title:                "打印调试输出"
html_title:           "Elixir: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在编写代码时，打印调试信息是一种常见的做法。它可以帮助程序员了解代码的执行过程，找出潜在的错误和问题。打印调试信息是一种简单而有效的调试工具，许多程序员都会使用它来提高代码的质量。

## 如何：

```
Elixir IO.puts("Debug output")
```

以上代码将在控制台输出 "Debug output"。程序员可以根据需要将任何变量或表达式放在 ```IO.puts()``` 中，以便在调试时查看其值。

## 深入探讨：

打印调试信息是一种老式的调试技术，它可以追溯到早期的编程语言。除了使用打印语句之外，现代语言也提供了许多其他调试工具，例如调试器和日志记录器。尽管如此，打印调试信息仍然是一种有效的方法，它简单易用，不需要使用任何特殊工具。

## 参考链接：

- [Elixir官方文档](https://elixir-lang.org/getting-started/io-and-the-file-system.html)
- [打印调试技术的历史](https://www.informit.com/articles/article.aspx?p=366893)
- [调试工具的比较](https://medium.com/@Zaccc123/debugging-elixir-an-overview-of-tools-ba38b7e38e1e)