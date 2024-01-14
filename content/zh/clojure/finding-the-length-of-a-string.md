---
title:    "Clojure: 寻找字符串的长度"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 为什么

在日常编程中，经常会遇到需要计算字符串的长度的情况。这对于处理文本数据来说很重要，可以帮助我们快速获得字符串的长度信息，方便我们进行后续的操作。在Clojure中，我们可以通过几行简单的代码来获得字符串的长度，下面将介绍具体的方法。

## 如何进行操作

在Clojure中，我们可以使用`count`函数来计算字符串的长度。下面是一个示例代码，假设我们需要计算字符串`Hello World`的长度：

```Clojure
(count "Hello World")

;; 输出结果为 11
```

我们可以看到，在`count`函数中直接传入要计算长度的字符串作为参数即可。值得注意的是，该函数也可以用来计算其他类型的数据的长度，比如列表、向量等。

```Clojure
(count [1 2 3 4])

;; 输出结果为 4
```

## 深入探讨

事实上，`count`函数本质上是`seq`和`reduce`两个函数的组合，它会先将字符串转换为序列，并将每个字符传递给`reduce`函数进行累计计算。因此，当我们传入一个空字符串时，输出的结果也会是0。

```Clojure
(count "")

;; 输出结果为 0
```

此外，`count`函数也支持Unicode字符，在计算时会将一个Unicode字符视为一个字符。

```Clojure
(count "你好")

;; 输出结果为 2
```

## 参考资料

- [Clojure Docs: count](https://clojuredocs.org/clojure.core/count)
- [Clojure Style Guide: Favor `count` Over `(.length s)`](https://github.com/bbatsov/clojure-style-guide#strings)
- [Clojure for the Brave and True: Strings](https://www.braveclojure.com/strings/)

---

## 参见

- [Clojure Docs: seq](https://clojuredocs.org/clojure.core/seq)
- [Clojure Docs: reduce](https://clojuredocs.org/clojure.core/reduce)
- [Clojure Docs: Unicode](https://clojuredocs.org/clojure.string/unicode)