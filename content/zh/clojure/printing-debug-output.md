---
title:    "Clojure: 打印调试输出"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

为什么：在编程过程中，打印调试输出是一种常用的技术。通过打印调试输出，我们可以更有效地跟踪代码的执行，发现和解决错误。

如何做：在Clojure中，我们可以使用`println`函数来打印调试输出。例如：

```Clojure
(defn add [a b]
  (println "Adding" a "and" b)
  (+ a b))

(add 2 3)
```

输出结果：

```Clojure
Adding 2 and 3
=> 5
```

可以看到，通过打印调试输出，我们可以清楚地看到代码的执行过程，以及每个变量的值。这有助于我们定位代码中存在的问题。

深入了解：除了`println`函数外，Clojure还提供了其他一些打印调试输出的函数，比如`prn`和`printf`。我们也可以使用`clojure.tools.logging`库来实现更复杂的调试功能。同时，我们也可以通过配置日志记录器来将调试信息保存到日志文件中。

[See Also (看看其他相关文章)](https://clojure.org/reference/logging)