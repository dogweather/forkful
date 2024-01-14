---
title:                "Gleam: 打印调试输出"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

#为什么
在编写和调试程序时，打印调试输出是非常有用的工具。它可以帮助程序员跟踪代码的执行过程，定位错误的位置并快速解决问题。通过打印调试输出，程序员可以更加直观地了解程序的运行情况，从而更有效地调试代码。

##如何做
在Gleam中，通过使用```print_debug()```函数来打印调试输出。该函数接受一个参数，可以是任意类型的变量或表达式。例如，我们想要打印一个字符串变量的内容，可以使用以下代码：

```Gleam
let message = "Hello world"
print_debug(message)
```

以上代码的输出结果将是：

```
"Hello world"
```

除了打印变量的值，我们还可以在```print_debug()```函数中使用表达式。例如，我们想要打印两个数的和，可以使用以下代码：

```Gleam
let a = 3
let b = 5
print_debug(a + b)
```

输出结果将是：

```
8
```

##深入探讨
除了基本的打印功能，```print_debug()```函数还有其他的选项可以使用。例如，我们可以使用```print_debug("Some text", debug_as_warning: true)```来打印一个警告信息，将内容显示为黄色。另外，我们也可以通过指定```debug_scope```参数来控制打印信息的范围，例如只在特定条件下才打印调试输出。

值得注意的是，打印调试信息可能会影响程序的性能。因此，在调试结束后，记得将不必要的调试输出语句删除。

#相关阅读
- [Gleam官方文档-打印调试输出](https://gleam.run/documentation/printing_debug_output)
- [Gleam官方博客-调试代码的最佳实践](https://blog.gleam.run/coding-best-practices-debugging)