---
title:                "Clojure: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##为什么会有将字符串转为小写的需求

在编程中，有时候我们会遇到需要将字符串转换为小写的情况。这可能是因为需要统一字符串的大小写形式，或者是为了比较字符串时可以忽略大小写的差异。无论是哪种情况，将字符串转换为小写都有其必要性。

##如何将字符串转为小写

```Clojure
(.toLowerCase "HELLO") ;; => "hello"
(.toLowerCase "hELlO wOrLd") ;; => "hello world"
```

通过调用`toLowerCase`函数，可以将字符串转换为小写形式。在Clojure中，字符串也是一种数据类型，所以我们可以直接对字符串使用`.`操作符来调用Java中的方法。以上是两个示例，分别将"HELLO"和"hELlO wOrLd"转换为小写形式。

##深入探讨字符串转为小写的过程

在Clojure中，字符串是以UTF-16编码存储的，所以转换为小写实际上是对Unicode字符的转换。Unicode中定义了每个字符的大小写形式，所以将字符串转换为小写时，实际上是将字符串中的每个字符都按照Unicode规范进行转换。

##另请参阅

- [Java String toLowerCase() method](https://www.javatpoint.com/java-string-tolowercase)
- [Unicode Character Database](https://unicode.org/ucd/)
- [Clojure String Functions](https://clojure.org/reference/strings)