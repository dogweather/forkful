---
title:    "Clojure: 打印调试输出"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

# 为什么
有时候，在编写代码的过程中，我们可能会遇到一些难以解决的问题。这时，打印调试输出信息可以帮助我们更好地了解程序运行的情况，从而更轻松地找到问题的解决方法。

# 如何做
```Clojure
(defn calculate-avg [numbers]
  (let [total (reduce + numbers)]
    (println "Total: " total)
    (println "Number of numbers: " (count numbers))
    (/ total (count numbers))))

(calculate-avg [2 4 6 8])
```
输出结果:
```
Total: 20
Number of numbers: 4
5
```

# 深入讨论
打印调试输出信息是一种简单有效的调试工具。除了在代码中直接使用`println`函数打印信息外，我们还可以使用`clojure.tools.logging`库提供的更多功能，比如在不同的日志级别打印信息。此外，使用调试器也是一种常用的调试方法。

# 参考链接
- [Markdown语法指南](https://www.markdownguide.org/basic-syntax/)
- [Clojure官方文档](https://clojure.org/documentation)
- [clojure.tools.logging库文档](https://github.com/clojure/tools.logging)