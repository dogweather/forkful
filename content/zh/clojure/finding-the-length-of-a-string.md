---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？

在编程中，找出字符串的长度就是找出它包含的字符个数。这对于很多事情来说都很重要，比如检查输入的有效性，或者在内存中合理分配位置。

## 如何实现：

在Clojure中，我们使用`count`函数来计算字符串的长度。以下是一个简单的例子：

```Clojure
(defn string-length [s]
  (count s))

(string-length "你好，世界") ;; 输出：5
```

在上面的代码中，我们定义了一个函数`string-length`，它接受一个字符串`s`作为输入，并返回字符串的长度。

## 深度了解

在早期的编程语言中，通常使用一个特殊的字符（比如 null 或 `\0`）来标记字符串的结束。但这就意味着我们计算字符串的长度时必须遍历整个字符串。Clojure使用了一个更快的方法：它在内部自动跟踪了字符串的长度，所以我们可以直接获得。

除了`count`外，Clojure还有很多其他处理字符串的函数。例如，我们可以用`subs`函数取子字符串，用`str`函数连接字符串。

`count`函数的实现很简单，它直接调用了Java的字符串方法`length`。这意味着其实我们也可以在Clojure中直接调用`.length`：

```Clojure
(.length "你好，世界") ;; 输出：5
```

## 参考资料

1. Clojure官方文档：[https://clojure.org/]
2. 关于Java的字符串函数：[https://docs.oracle.com/javase/7/doc/api/java/lang/String.html]
3. 更多关于Clojure处理字符串的课程：[https://www.learn-clojure.com/strings]