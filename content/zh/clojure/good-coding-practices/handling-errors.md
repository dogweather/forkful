---
title:                "处理错误"
aliases: - /zh/clojure/handling-errors.md
date:                  2024-01-26T00:51:06.516307-07:00
model:                 gpt-4-1106-preview
simple_title:         "处理错误"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/handling-errors.md"
---

{{< edit_this_page >}}

## 什么与为什么？
错误处理就是管理程序中的意外情况——就像门卫处理闹事者一样。程序员喜欢一切顺利；错误处理有助于控制麻烦事，确保他们的代码在面对意外时不会跌跤。

## 如何操作：
Clojure，像它的Lisp祖先一样，依靠异常来处理错误。以下是当事情朝不好的方向发展时，你如何显示你的处理能力。

抛出异常很直接：
```Clojure
(throw (Exception. "哎哟！出问题了。"))
```

捕获异常，你将经常这么做：
```Clojure
(try
  ;; 风险代码
  (/ 1 0)
  (catch ArithmeticException e
    (println "不能除以零！"))
  ;; 无论如何 finally 块都会运行
  (finally 
    (println "清理代码写在这里。")))
```
以上 catch 块的示例输出：
```
不能除以零！
清理代码写在这里。
```

使用 `ex-info` 和 `ex-data` 提供关于异常的更丰富上下文：
```Clojure
(try
  ;; 引发一个自定义异常
  (throw (ex-info "自定义错误" {:type :custom-failure}))
  (catch Exception e
    ;; 从我们的自定义异常中获取数据
    (println (ex-data e))))
```
示例输出：
```
{:type :custom-failure}
```

## 深入探讨
Clojure中的错误处理与其他Lisps或甚至Java（其继承了 `try-catch` 机制）并无根本的不同。它是实用的；使用异常是主要路径，就像Java一样，但Clojure通过 `ex-info` 和 `ex-data` 提供了一个具有函数式风味的更丰富的错误数据。

Clojure中错误处理的替代方案包括使用单子构造，如 `cats` 等库中的 `either` 单子，或 core.async 用于基于通道的错误传播。然而，这些更复杂，通常用于特定场景。

从历史上看，编程语言中的错误处理已从简单的状态返回发展到现代语言中更复杂的异常处理机制。Clojure选择了简单性和一点函数式编程的味道，融合了新旧。

## 参见
- Clojure的异常指南：https://clojure.org/guides/exceptions
- “Cats” 库，用于更多函数式方法：https://github.com/funcool/cats
- “Core.async” 用于异步编程：https://github.com/clojure/core.async
