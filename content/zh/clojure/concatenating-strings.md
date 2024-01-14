---
title:                "Clojure: 字符串拼接"
simple_title:         "字符串拼接"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

# 为什么
字符串拼接是编程中常用的技巧，它能够将多个字符串连接在一起，形成一个新的字符串。这在处理文本和输出信息时非常有用。在Clojure中，我们可以通过一些简单的方法来实现字符串拼接，让我们来看看如何做到这一点。

## 如何
在Clojure中，字符串拼接的最基本方法是使用`str`函数。这个函数接受任意数量的参数，并将它们连接成一个新的字符串。让我们来看一个简单的例子：
```Clojure
(str "Hello" " " "World") 
```
这将输出`Hello World`。我们也可以将变量包含在拼接中，如下所示：
```Clojure
(def name "John")
(str "Hello" " " name) 
```
这将输出`Hello John`。如果需要，我们也可以在拼接中添加空格或其他字符来分隔字符串。

另一种常用的拼接方法是使用`str-join`函数。这个函数接受两个参数：分隔符和一个字符串的集合，然后将集合中的字符串使用分隔符连接起来。例如：
```Clojure
(def fruits ["apple" "banana" "orange"])
(str-join ", " fruits) 
```
这将输出`apple, banana, orange`。我们也可以通过添加条件来跳过某些字符串，如下所示：
```Clojure
(str-join ", " (filter even? [1 2 3 4 5])) 
```
这将输出`2, 4`，因为`filter`函数将集合中的偶数过滤掉，然后再进行拼接。

## 深入了解
在Clojure中，字符串是不可变的数据类型，这意味着我们不能直接修改已有的字符串。因此，当我们进行字符串拼接时，实际上是在创建一个新的字符串，并将原有的字符串复制到新的字符串中。

此外，字符串拼接的效率也值得考虑。在拼接大量字符串时，每次创建新的字符串会消耗更多的时间和内存。因此，我们可以考虑使用`string-builder`函数来提升性能。这个函数可以将多个字符串缓存起来，然后在需要时一次性拼接到一个新的字符串中。

# 参考链接
- [Clojure字符串函数官方文档](https://clojuredocs.org/clojure.string)
- [Clojure字符串拼接方法](https://stackify.com/clojure-string-join-concat/)
- [Clojure字符串拼接的性能优化技巧](http://blog.rancher.com/clojure-performance-tuning-for-string-concatenation/)