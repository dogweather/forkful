---
title:                "“写入标准错误”"
html_title:           "Clojure: “写入标准错误”"
simple_title:         "“写入标准错误”"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

《为什么要向标准错误写入？》

有时候在编程过程中，我们会遇到需要输出错误信息的情况。这时候，使用标准错误输出可以让我们更方便地找到和处理错误，从而提高代码的健壮性。

《如何写入标准错误》

```Clojure
;; 使用`clojure.java.io`库中的`error-writer`函数来创建一个向标准错误输出的写入器
(def stderr-writer (error-writer))

;; 使用`write`函数来向标准错误写入信息
(write stderr-writer "Uh oh, something went wrong!")

```

输出：

```
Uh oh, something went wrong!
```

《深入了解标准错误输出》

除了使用`write`函数来向标准错误输出信息，我们还可以使用`throw`函数来抛出错误，并让Clojure自动将其输出到标准错误。此外，我们还可以通过`binding`宏来设置当前线程的`*err*`变量，从而将标准错误输出重定向到其他地方。

《相关阅读》

- Clojure官方文档: https://clojure.org/
- Clojure标准库文档: https://clojuredocs.org/
- `clojure.java.io`文档: http://clojuredocs.org/clojure.java.io
- 《The Joy of Clojure》: https://www.manning.com/books/the-joy-of-clojure