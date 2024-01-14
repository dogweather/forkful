---
title:    "Clojure: 删除匹配模式的字符"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 为什么要删除匹配某模式的字符？

在编程中，我们经常会遇到需要删除特定字符或模式的情况。这可能是因为我们需要清理数据，或者需要从字符串中提取特定信息。无论何种原因，删除字符是一个常见的编程任务。在Clojure中，我们可以使用简洁的代码来快速有效地删除字符，让我们来看看具体的方法。

## 如何实现

Clojure提供了多种方法来删除字符。最常用的方法是使用`clojure.string`库中的`replace`函数。我们可以将目标字符串和要删除的字符匹配模式作为参数传递给`replace`函数，它将返回一个新的字符串，其中匹配模式的字符被删除了。 例如，假设我们有一个字符串"Hello World!"，我们想要删除所有的小写字母。我们可以这样写代码：

```Clojure
(def str "Hello World!")

(clojure.string/replace str #"[a-z]" "")
```

这里，我们首先定义了一个字符串变量`str`，然后使用`replace`函数将所有小写字母（通过正则表达式`[a-z]`匹配）替换为空字符串。执行后，输出将是："HW"。

除此之外，我们还可以使用`clojure.string`中的其他函数，如`replace-first`来删除首个匹配字符，以及`replace-last`来删除最后一个匹配字符。

## 深入了解

除了`clojure.string`中提供的函数外，我们也可以使用`clojure.core`库中的`subs`函数来删除指定位置的字符。`subs`函数接受一个字符串和一个范围，可以是字符位置的起始和结束位置，也可以是两个字符之间的关系。例如：

```Clojure
(def str "Hello World!")

(clojure.core/subs str 1 5)
```

这里，`subs`函数将从第二个字符（1）开始，删除到第六个字符（5）之前的所有字符。因此，输出为"ello"。

另外，我们也可以使用`drop-while`和`take-while`函数来根据指定的条件来删除字符。例如，我们可以使用`drop-while`函数来删除所有数字：

```Clojure
(def str "H3ll0 W0rld!")

(clojure.core/concat (clojure.core/drop-while #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} str))
```

这里，我们首先定义了一个字符串变量`str`，然后使用`drop-while`函数来删除所有数字。最后，我们使用`concat`函数来将删除后的字符串连接起来，输出为"Hll Wrd!"。

## 参考链接

- [Clojure官方文档](https://clojure.org/api/cheatsheet)
- [Clojure字符串操作教程](http://gigbook.site/clojure-guide/strings.html)
- [Clojure库中字符串函数的使用](https://clojure.org/api/cheatsheet#string_functions)