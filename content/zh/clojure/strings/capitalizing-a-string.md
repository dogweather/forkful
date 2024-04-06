---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:56.244219-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Clojure\u4F5C\u4E3A\u4E00\u79CDJVM\u8BED\
  \u8A00\uFF0C\u5141\u8BB8\u4F60\u76F4\u63A5\u4F7F\u7528Java String\u65B9\u6CD5\u3002\
  \u8FD9\u91CC\u6709\u4E00\u4E2A\u5982\u4F55\u5728Clojure\u4E2D\u5C06\u5B57\u7B26\u4E32\
  \u9996\u5B57\u6BCD\u5927\u5199\u7684\u57FA\u672C\u793A\u4F8B\uFF1A."
lastmod: '2024-04-05T21:53:47.636434-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 如何操作：
Clojure作为一种JVM语言，允许你直接使用Java String方法。这里有一个如何在Clojure中将字符串首字母大写的基本示例：

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "hello world!") ; => "Hello world!"
```

Clojure没有包含一个专门用于字符串首字母大写的内建函数，但如所示，你可以通过组合`clojure.string/upper-case`、`subs`和`str`函数轻松实现这一点。

对于更简洁的解决方案和处理更复杂的字符串操作，你可能会转向第三方库。在Clojure生态系统中，一个此类受欢迎的库是`clojure.string`。然而，根据我最后的更新，它没有提供一个直接的`capitalize`函数超出核心Clojure功能所演示的，因此上面显示的方法是你不引入专门用于首字母大写的额外库的直接方法。

记住，当在Clojure中处理与Java方法交互的字符串时，你实际上是在处理Java字符串，如果必要，使你能够在你的Clojure代码中直接利用Java的String方法的全部武器库。
