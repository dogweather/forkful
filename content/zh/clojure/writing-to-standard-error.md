---
title:    "Clojure: 写入标准错误"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么要写到标准错误

编写到标准错误（standard error）是调试和测试中极其重要的一步。它允许程序员实时地观察代码的执行过程，并且能够捕捉到程序中出现的错误信息。这样可以帮助我们更快地定位和解决问题，提高代码质量。

## 如何做到

在Clojure中，我们可以使用`println`或`eprintln`函数来将信息输出到标准错误。下面是一个例子：

```Clojure
(eprintln "Hello world!")
```

程序会在运行时将`Hello world!`这条信息显示在终端中的标准错误流中。

## 深入探讨

除了简单的输出信息外，我们还可以将更复杂的信息结构化后输出到标准错误流中。比如，我们可以将信息封装为一个含有关键字的map，再使用`prn`函数将其输出。下面是一个例子：

```Clojure
(def error {:error-type "Runtime Error"
            :error-message "Division by zero"
            :error-line 5})

(eprn error)
```

输出结果为：

```
{:error-type "Runtime Error", :error-message "Division by zero", :error-line 5}
```

这样的结构化输出可以让我们更清晰地了解程序中出现的错误，方便调试和修复代码。此外，我们还可以使用`throw`函数将错误信息抛出到标准错误流中，让程序在遇到错误时停止执行。

## 参考链接

- [Clojure文档](https://clojure.org/)
- [关于标准错误流的介绍](https://www.linuxnix.com/fixing-the-golang-standard-error-traping-improve-your-effectiveness/) 
- [更多关于异常处理的方法](https://purelyfunctional.tv/guide/clojure-error-handling/)