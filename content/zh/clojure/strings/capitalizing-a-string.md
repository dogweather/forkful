---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:56.244219-07:00
description: "\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\u6D89\u53CA\u4FEE\u6539\
  \u5B57\u7B26\u4E32\uFF0C\u4F7F\u5176\u7B2C\u4E00\u4E2A\u5B57\u7B26\u4E3A\u5927\u5199\
  \uFF0C\u800C\u5B57\u7B26\u4E32\u7684\u5176\u4F59\u90E8\u5206\u4FDD\u6301\u4E0D\u53D8\
  \u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\u884C\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\
  \u5927\u5199\u64CD\u4F5C\uFF0C\u4EE5\u786E\u4FDD\u6570\u636E\u7684\u4E00\u81F4\u6027\
  \uFF0C\u7279\u522B\u662F\u5BF9\u4E8E\u540D\u79F0\u548C\u5730\u70B9\uFF0C\u6216\u8005\
  \u7B26\u5408\u7528\u6237\u754C\u9762\u7684\u8BED\u6CD5\u89C4\u5219\u3002"
lastmod: '2024-03-13T22:44:47.284916-06:00'
model: gpt-4-0125-preview
summary: "\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\u6D89\u53CA\u4FEE\u6539\
  \u5B57\u7B26\u4E32\uFF0C\u4F7F\u5176\u7B2C\u4E00\u4E2A\u5B57\u7B26\u4E3A\u5927\u5199\
  \uFF0C\u800C\u5B57\u7B26\u4E32\u7684\u5176\u4F59\u90E8\u5206\u4FDD\u6301\u4E0D\u53D8\
  \u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\u884C\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\
  \u5927\u5199\u64CD\u4F5C\uFF0C\u4EE5\u786E\u4FDD\u6570\u636E\u7684\u4E00\u81F4\u6027\
  \uFF0C\u7279\u522B\u662F\u5BF9\u4E8E\u540D\u79F0\u548C\u5730\u70B9\uFF0C\u6216\u8005\
  \u7B26\u5408\u7528\u6237\u754C\u9762\u7684\u8BED\u6CD5\u89C4\u5219\u3002."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 什么 & 为什么？
字符串首字母大写涉及修改字符串，使其第一个字符为大写，而字符串的其余部分保持不变。程序员经常执行字符串首字母大写操作，以确保数据的一致性，特别是对于名称和地点，或者符合用户界面的语法规则。

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
