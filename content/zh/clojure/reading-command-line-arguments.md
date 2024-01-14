---
title:                "Clojure: 从命令行读取参数"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么会读取命令行参数

读取命令行参数是一种常见的编程技术，可以帮助程序员在执行代码时从终端接收和处理用户输入。它能够提高程序的灵活性和交互性，使得程序更容易被用户使用。在本文中，我们将学习如何在Clojure中读取命令行参数以及深入了解这一过程。

# 如何做到

在Clojure中，我们可以使用```*command-line-args*```变量来读取命令行参数。它是一个字符串列表，其中每个元素都是一个命令行参数。让我们来看一个简单的示例代码：

```Clojure
(defn greet [name]
  (println "Hello" name))

(defn -main [& args]
  (doseq [arg *command-line-args*]
    (greet arg)))
```

这段代码定义了一个叫做```greet```的函数，用来打印出问候语。接着，我们在```-main```函数中使用了```doseq```循环来遍历```*command-line-args*```变量中的每个命令行参数，并将其作为参数传递给```greet```函数。现在，让我们从终端运行这段代码，并给它传入几个参数：

```
$ clj -m example.clj John Mary Bob

Hello John
Hello Mary
Hello Bob
```

可以看到，程序成功读取并处理了我们传入的命令行参数，并打印出了对应的问候语。你也可以在程序中根据需要使用这些参数来进行其他操作。

# 深入了解

在深入了解读取命令行参数的过程之前，我们需要了解一下命令行的基本结构。当我们在终端中输入一条命令时，比如```clj -m example.clj John Mary```，实际上是在告诉操作系统执行一个由多个单词（命令、参数等）组成的命令，而这些单词之间通过空格来分隔。因此，在Clojure中，我们可以通过访问```*command-line-args*```变量来获取这些单词，并使用它们来编写更加灵活和交互式的程序。

此外，我们可以通过使用```command-line-opts```函数来获取命令行中的可选参数。这个函数会将命令行中的可选参数转换为一个关联数组（可以理解为键值对集合），方便我们在程序中进行使用。如果你想要更深入了解如何使用```command-line-opts```函数，可以查看相关文档和实践一下。

# 参考链接

- [Clojure官方文档](https://clojure.org/)
- [Clojure Cookbook: Reading Command-Line Arguments](https://www.oreilly.com/library/view/clojure-cookbook/9781449366148/ch05.html)
- [Clojure Programming/Building Command Line Applications](https://en.wikibooks.org/wiki/Clojure_Programming/Building_Command_Line_Applications)