---
title:                "提取子字符串"
html_title:           "Clojure: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么和为什么？
提取子字符串是指从一个字符串中获取特定部分的操作。程序员通常之所以这样做，是因为他们需要对字符串进行分析或比较，而只对其中一部分感兴趣。

## 如何：
```Clojure
; 使用`subs`函数提取子字符串
(subs "你好，世界" 2 5) ; 输出 "好，世"

; 使用`nth`函数提取单个字符
(nth "HelloWorld" 5) ; 输出 "W"

; 使用关键字寻找字符串中的特定部分
(str/starts-with? "Clojure语言" :start) ; 输出 true
```

## 深入：
提取子字符串的操作在编程语言中很常见，它可以追溯到早起的字符串处理函数，如BASIC语言中的`LEFT`和`RIGHT`。除了Clojure中的`subs`和`nth`函数外，还有一些其他方法可用于提取子字符串，如使用正则表达式和Java的`substring`函数。在实现上，Clojure使用乱序列表数据结构来存储字符串，这使提取子字符串的操作更有效率。

## 参考链接：
- [Clojure官方文档](https://clojure.org/)
- [Java substring函数文档](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int))