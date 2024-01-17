---
title:                "将字符串转换为小写"
html_title:           "Clojure: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

什么是转换字符串为小写，为什么程序员要这样做？

转换字符串为小写是指将字符串中的所有字母转换为小写形式。程序员通常这样做是因为在编程中，对于比较和处理字符串来说，大小写通常是无关紧要的，因此将字符串统一转换为小写可以简化比较和处理的过程。

如何进行转换？

在Clojure中，可以使用下面的代码将字符串转换为小写并打印出来：

```Clojure
(def str "Hello World")
(.toLowerCase str)  ; 输出 "hello world"
```

另外，可以使用Clojure中的字符串函数 `clojure.string/lower-case` 来进行转换，如下所示：

```Clojure
(str "HELLO WORLD")   ; 输出 "hello world"
(clojure.string/lower-case "HELLO WORLD")  ; 输出 "hello world"
```

深入了解

转换字符串为小写的历史背景：在早期的编程语言中，大小写并不是无关紧要的，因此需要程序员手动进行大小写的转换。但随着现代编程语言的发展，许多语言都提供了内置的函数来进行大小写转换，从而简化了程序员的工作。

替代方法：除了使用上面提到的内置函数外，程序员也可以自己实现一个函数来进行大小写转换。另外，也可以使用正则表达式来匹配和替换字符串中的大写字母。

实现细节：在Clojure中，字符串是不可变的，因此在进行大小写转换时，会创建一个新的字符串对象。同时，底层使用的是Java中的 `toLowerCase` 或 `toUpperCase` 方法来实现大小写转换。

相关链接

更多关于Clojure字符串函数的信息，请参考官方文档：https://clojure.org/reference/strings

如果想要深入了解字符串和正则表达式的知识，可以查阅相关书籍或在线资源。