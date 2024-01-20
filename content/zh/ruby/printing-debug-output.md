---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

# 打印调试输出基础教程（Ruby版）

## 什么和为什么？
打印调试输出是一种在代码执行过程中查看运行情况的方法。编程人员使用它来跟踪代码运行过程中发生的变化，帮助发现和解决问题。

## 如何操作:

以下是如何在Ruby中打印调试输出的例子。

```Ruby
def add_two_numbers(num1, num2)
  result = num1 + num2
  puts "调试: 结果是 #{result}"
  result
end

add_two_numbers(2, 3)
```

运行后，你会在终端看到“调试:结果是5”。

## 深入学习

打印调试输出的方法有了很长的历史，不同的编程语言有不同的实现方式。在Ruby中，你可以选择`puts`或者`p`来输出。`puts`经常用于在终端打印人类可读的信息，而`p`则在调试中扮演重要角色，因为它可以输出更详细的信息。

除了`puts`和`p`，你还可以使用更高级的调试库，例如`byebug`。它提供更复杂但更强大的方法，如单步执行和断点设置，帮助你更好地调试程序。

在实现细节上，调试输出通常在终端中打印，可以直接通过运行程序的STDOUT获取到。Ruby的`puts`和`p`命令也是如此。

## 相关资源

- Ruby官方文档：[Kernel Module](https://ruby-doc.org/core-3.0.1/Kernel.html)——这个模块包含`puts`和`p`命令的官方文档。
- [byebug](https://github.com/deivid-rodriguez/byebug)——这是一个强大的Ruby调试器，你可以在GitHub上找到关于它的所有信息。