---
title:                "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

##为什么

当我们在编写Clojure程序时，我们经常需要打印出一些调试信息来帮助我们理解程序的运行情况。这些信息可以帮助我们查找错误和调试代码，从而提高代码的质量和可靠性。

##如何

在Clojure中，我们可以使用`println`函数来打印调试信息。下面是一个简单的示例：

```
Clojure
(defn add [a b]
  (println "正在计算 " a " + " b)
  (+ a b))
```

当我们调用这个函数时，它会打印出类似于`正在计算 3 + 5`的信息，让我们知道程序正在执行加法运算。我们还可以使用`format`函数来格式化打印的信息，使其更易读。

##深入了解

除了使用`println`和`format`函数，我们还可以使用各种其他方法来打印调试信息。例如，我们可以使用`clojure.tools.logging`库来记录信息，并在日志文件中查看它们。我们还可以使用`clojure.repl`库中的`pst`函数来打印出函数调用栈，帮助我们找到错误的位置。

##看看这些

- [Clojure.tools.logging文档](https://clojuredocs.org/clojure.tools.logging)
- [Clojure.repl文档](https://clojuredocs.org/clojure.repl)