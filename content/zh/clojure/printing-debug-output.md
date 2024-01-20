---
title:                "打印调试输出"
html_title:           "Clojure: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## 是什么？为什么？ (What & Why?)
打印调试输出是在代码中添加临时语句以查看内部操作。程序员这样做是为了更好地理解程序运行的情况，帮助修复错误。

## 怎么做？ (How to?)
在 Clojure中，我们使用 `(println ...)` 将变量打印到控制台。给它提供你要打印的内容作为参数。
```Clojure
(def foobar 22)

(println foobar)  ;; Print variable to console

;; Output:
;; 22
```
通过 `println` ，你可以将任何变量，甚至是复杂的数据结构，打印到控制台。

## 深入研究 (Deep Dive)
打印调试是早期程序出错的主要排查方式。尽管今天有更复杂的调试工具，例如断点和利用调试器步进调试，但它仍然具有价值，尤其是对那些编写简单脚本或者偏好命令行的程序员。

Clojure其他的调试输出选项包括 logging 库，允许你输出到文件或远程系统。你还可以使`print`和 `prn` ，这两者分别不换行和使用 Clojure 的读函数输出。

值得注意的是，`println` 或许比你想象的更复杂。实际上，它使用 Java 的 `System.out.println`，将指定的值转换为字符串并在末尾添加换行符。这是通过调用 `str` 函数实现的，这也说明了为什么你可以在 `println` 中使用多种类型的值。

## 参考资料 (See Also)
1. Clojure 官方文档说明 `println`: https://clojuredocs.org/clojure.core/println
2. `prn`函数的更多信息: https://clojuredocs.org/clojure.core/prn
3. 关于 Clojure 的 logging 库信息: https://github.com/clojure/tools.logging
4. 更多关于 Clojure 的 `str` 函数和它如何转换各种类型的信息: https://clojuredocs.org/clojure.core/str