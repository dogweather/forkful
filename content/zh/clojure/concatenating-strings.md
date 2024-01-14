---
title:    "Clojure: 连接字符串"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

为什么要连接字符串？

在编程中，我们经常需要将多个字符串合并成一个字符串，这样可以更轻松地处理和操作文本数据。通过连接字符串，我们可以创建新的文本变量，方便后续的使用和处理。

如何实现字符串连接？

要连接字符串，在Clojure中使用`str`函数是最简单的方法。该函数接受任意数量的字符串参数，并返回一个连接起来的新字符串。例如：

```Clojure
(str "Hello" " " "World")
```

输出：Hello World

我们也可以使用`.`操作符来连接字符串，比如`(.concat "Hello" " " "World")`，但是这种方法只能连接两个字符串。因此，在需要连接多个字符串时，推荐使用`str`函数。

深入了解连接字符串

除了`str`函数，Clojure还提供了一些其他的方法来处理连接字符串。比如使用`join`函数来通过指定分隔符连接一个字符串集合，使用`subs`函数来获取字符串的子串，以及使用`format`函数来按照指定格式连接字符串。

另外，Clojure还有一个`StringBuilder`类，可以在需要高性能连接大量字符串时使用。该类是可变的，可以避免频繁创建新的字符串，从而提高性能。

还有一点需要注意的是，在连接大量字符串时，最好避免使用`+`操作符，因为每次使用`+`操作符都会创建一个新的字符串对象，占用更多的内存空间。

## 查看更多

- [Clojure官方文档](https://clojure.org) 
- [Clojure Cookbook中关于字符串连接的章节](https://www.clojurecookbook.org/#strings)