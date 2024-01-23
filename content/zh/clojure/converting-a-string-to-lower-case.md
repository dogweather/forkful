---
title:                "将字符串转换为小写"
date:                  2024-01-20T17:38:00.437516-07:00
model:                 gpt-4-1106-preview
simple_title:         "将字符串转换为小写"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
将字符串转换为小写就是把所有的字母字符变成小写形式。程序员这么做通常是为了数据比较或确保数据一致性，比如在搜索和排序时。

## How to: 如何操作
```Clojure
; 使用clojure.string/lower-case函数
(require '[clojure.string :as str])

(defn to-lower-case [input-str]
  (str/lower-case input-str))

(println (to-lower-case "Hello, World!")) ; 输出 "hello, world!"
```

## Deep Dive 深入了解
早期编程语言就引入了将字符串转换为小写的操作，主要目的是为了文本处理的需要。在Clojure中，`clojure.string/lower-case`函数是处理这一任务的直接方式。这个函数在内部使用Java的`toLowerCase()`，因为Clojure是建立在JVM上的。

替代方案包括使用正则表达式或者遍历字符串，手动将每个字符转换成小写。然而，这些办法通常效率低下，不如使用现成的库函数。

考虑国际化时，字符串转换需要注意特定语言的规则。比如，在土耳其语中，字符'I'小写不是'i'，而是'ı'。`clojure.string/lower-case`函数能够正确处理这些情况，因为它依赖Java的`toLowerCase()`，该函数考虑了语言环境。

## See Also 另请参阅
- Clojure字符串API文档: [String Functions](https://clojuredocs.org/clojure.string)
- Java `toLowerCase()`方法: [Java String toLowerCase()](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#toLowerCase())
