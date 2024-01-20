---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么和为什么？
子字符串提取是将字符串中的一部分单独取出，以编程语言的角度来看，我们需要抽取子字符串的地方非常多，例如：解析文本、日志记录、密码验证等。

## 如何操作：
在Clojure中，我们使用`subs`函数来提取子字符串，下面有一些例子：

```Clojure
(def str "Hello, Clojure!")

; 从索引5开始
(subs str 5)
=> ", Clojure!"

; 从索引7到索引13
(subs str 7 13)
=> "Clojur"
```

## 深入探讨：
Clojure的`subs`函数的历史可以追溯到Java的 `substring` 函数，这两个函数的工作原理非常相似。除了 `subs` 函数，我们还可以使用正则表达式来提取子字符串。而且，尽管 `subs` 函数看起来很简单，但它的背后有很复杂的实现细节。例如，`subs` 是基于Java的 `substring` 的，这意味着当我们从大字符串中提取小字符串时，大字符串可能不会被垃圾回收，这可能会导致内存问题。

## 另请参阅：
对于更深入的阅读和学习，我推荐这些链接：
- Clojure官方文档：https://clojuredocs.org/clojure.core/subs
- Java的String：https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
- Regex One：https://regexone.com/lesson/extracting_data
记住：永远善用官方文档和教程，这是理解和精通编程语言的关键。