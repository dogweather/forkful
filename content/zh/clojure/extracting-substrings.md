---
title:                "Clojure: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

提取字符串是编程中经常用到的操作，它可以让我们从一个字符串中获取我们需要的部分内容。在Clojure中，提取子字符串的方法也很简单，让我们来看看如何实现！

## 如何操作

首先，我们需要使用`subseq`函数来提取子字符串，它的语法如下：

```
(subseq string start-pos end-pos?)
```

其中，`string`表示原始字符串，`start-pos`表示子字符串的起始位置，`end-pos?`表示子字符串的结束位置。需要注意的是，`end-pos?`是可选参数，如果不指定，默认为原始字符串的末尾。下面是一个使用示例：

```
(def s "Hello, world!")
(subseq s 2 5)
```

以上代码将会提取`"llo"`作为输出结果。此外，Clojure也提供了更简便的方式来提取子字符串，比如使用`subs`函数：

```
(subs string start-pos?)
```

在这种情况下，我们只需要指定起始位置即可，`end-pos?`会被自动设置为原始字符串的末尾。下面是同样的例子，使用`subs`函数：

```
(def s "Hello, world!")
(subs s 2)
```

输出结果仍然是`"llo"`。除了使用数字来指定位置，我们也可以使用关键字来提取子字符串，比如使用`start`和`end`关键字：

```
(def s "Hello, world!")
(subs s :start 2 :end 5)
```

同样，输出结果也是`"llo"`。此外，我们还可以使用负数来表示位置，比如`-1`表示倒数第一个字符，`-2`表示倒数第二个字符，依此类推。

## 深入了解

除了上述的方法外，我们还可以使用`re-find`函数来从一个字符串中匹配正则表达式，并提取匹配的部分作为子字符串。例如，以下代码可以从原始字符串中提取所有的数字：

```
(def s "1a2b3c4d5e")
(re-find #"\d+" s)
```

输出结果是`1`，`2`，`3`，`4`，`5`，每个数字占一行。如果我们想要将这些数字放在一个列表中，可以使用`re-seq`函数：

```
(def s "1a2b3c4d5e")
(re-seq #"\d+" s)
```

输出结果是一个包含所有数字的列表：`(1 2 3 4 5)`。

## 查看更多

如果你想要了解更多有关提取子字符串的信息，你可以参考下面这些链接：

- [Clojure字符串操作文档](https://clojure.org/reference/java_interop#_string_operations)
- [Clojure正则表达式文档](https://clojuredocs.org/clojure.core/re-find)
- [Java String类文档](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)

## 看看其他文章

如果你对Clojure编程有兴趣，可以阅读下列文章了解更多：

- [Clojure数据结构简介](https://www.importnew.com/11963.html)
- [Clojure多线程编程指南](https://www.ibm.com/developerworks/cn/java/j-clojure-parallel-function/index.html)
- [Clojure编程入门指南](https://clojure.org/guides/getting_started)