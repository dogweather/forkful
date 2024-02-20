---
date: 2024-01-20 17:38:00.437516-07:00
description: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199\u5C31\u662F\u628A\
  \u6240\u6709\u7684\u5B57\u6BCD\u5B57\u7B26\u53D8\u6210\u5C0F\u5199\u5F62\u5F0F\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u901A\u5E38\u662F\u4E3A\u4E86\u6570\u636E\u6BD4\
  \u8F83\u6216\u786E\u4FDD\u6570\u636E\u4E00\u81F4\u6027\uFF0C\u6BD4\u5982\u5728\u641C\
  \u7D22\u548C\u6392\u5E8F\u65F6\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.370600
model: gpt-4-1106-preview
summary: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199\u5C31\u662F\u628A\
  \u6240\u6709\u7684\u5B57\u6BCD\u5B57\u7B26\u53D8\u6210\u5C0F\u5199\u5F62\u5F0F\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u901A\u5E38\u662F\u4E3A\u4E86\u6570\u636E\u6BD4\
  \u8F83\u6216\u786E\u4FDD\u6570\u636E\u4E00\u81F4\u6027\uFF0C\u6BD4\u5982\u5728\u641C\
  \u7D22\u548C\u6392\u5E8F\u65F6\u3002"
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
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
