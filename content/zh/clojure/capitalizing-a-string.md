---
title:                "将字符串大写化"
html_title:           "Clojure: 将字符串大写化"
simple_title:         "将字符串大写化"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么是大写字符串？为什么程序员这么做？

大写字符串是将一个字符串中的每个字母都变成大写形式的过程。程序员通常会这么做是为了统一字符串的格式，使其更易于比较和处理。

## 如何实现大写字符串：

```Clojure
(defn capitalize-str [str]
  (String/upper-case str)) ;使用Java内置函数将字符串转换成大写形式

(capitalize-str "hello world") ;输出结果为"HELLO WORLD"
```

## 深入探讨：

大写字符串的概念并非令人惊讶，因为在自然语言中，大写字母通常用于标识词的开头。在编程语言中，大写字符串可用于忽略大小写进行比较，以及将字符串格式化为特定的约定格式。除了使用Java内置函数，我们也可以通过使用正则表达式和循环来实现大写字符串，但这可能会增加代码复杂性。

## 参考资料：

了解更多关于Clojure的字符串处理方法可以参考官方文档：https://clojure.org/reference/strings