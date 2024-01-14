---
title:    "Clojure: 提取子串"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# 为什么要提取子字符串？

提取子字符串是一种在编程中经常用到的技巧。当我们处理大量文本数据时，经常需要从中提取出特定的部分，比如搜索关键词或者截取需要的信息。提取子字符串是一种高效的方法，可以帮助我们快速地处理文本数据，提取出我们所需要的信息。

# 如何提取子字符串

在Clojure中，提取子字符串非常简单。我们可以使用`subs`函数来提取指定的子字符串。以下是一个例子：

```Clojure
(def str "Hello World!")
(subs str 0 5) ; 输出 "Hello"
```

在上面的例子中，我们首先定义了一个字符串变量`str`，然后使用`subs`函数来提取从索引0到索引5的子字符串。注意，字符串的索引是从0开始的。

我们也可以使用负数来表示索引，例如`subs str 6 -1`表示从索引6开始，一直到字符串结尾的子字符串。此外，我们还可以使用`:start`和`:end`关键字来指定开始和结束的索引。

# 深入了解提取子字符串

除了基本的`subs`函数外，Clojure中还有其他一些函数可以用来提取子字符串。比如，`subs-pos`函数可以根据指定的正则表达式来提取子字符串。`subvec`函数可以提取子向量。

此外，我们还可以在字符串上使用`re-seq`函数来提取匹配正则表达式的所有子字符串。下面是一个例子：

```Clojure
(def str "good morning, everybody!")
(def matches (re-seq #"[a-z]+" str)) ; 提取所有小写字母的子字符串
; 输出 ("good" "morning" "everybody")
```

总的来说，提取子字符串是一种非常常用的技巧，对于处理文本数据非常有帮助。在Clojure中，有多种方法可以实现提取子字符串的功能，我们可以根据具体的需求选择最合适的方法。

# 参考链接

- [`subs`函数文档](https://clojuredocs.org/clojure.core/subs)
- [`re-seq`函数文档](https://clojuredocs.org/clojure.core/re-seq)
- [正则表达式教程](https://www.runoob.com/regexp/regexp-tutorial.html)