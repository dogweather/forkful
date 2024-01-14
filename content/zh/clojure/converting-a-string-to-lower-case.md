---
title:                "Clojure: 将字符串转换为小写"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么？

降低字符大小写是一项常见的编码任务，它允许您将输入的字符串转换为小写形式。这在处理用户输入，比较字符串以及其他数据处理任务时非常有用。

## 如何？

```Clojure
(let [input "Hello World!"]
  (clojure.string/lower-case input))
```

这段代码将字符串`Hello World!`转换为小写，并返回输出`hello world!`。您还可以在此代码块中尝试其他字符串来测试不同的输出。

## 深入挖掘

转换字符串的API包括`clojure.string/lower-case`，以及`clojure.string/lower-case*`（允许您指定特定的区域设置）。这些函数都是非常高效的，并且可以处理包含非英语字符的字符串。您还可以使用`String/lowerCase` Java方法来实现相同的功能。

## 参考

【深入了解Clojure开发语言】https://www.clojure.org/

【Clojure字符串操作文档】https://clojuredocs.org/clojure.string/lower-case

【Java String类文档】https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#lowerCase--