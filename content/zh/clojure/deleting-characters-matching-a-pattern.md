---
title:    "Clojure: 删除匹配模式的字符"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 为什么

Clojure 是一种功能强大的编程语言，它的设计目标就是简洁、高效和具有表达力。在日常开发过程中，删除与特定模式匹配的字符是一个常见的操作。这样一来，可以根据需求来过滤字符串并提取想要的部分。接下来我们将一起探讨如何在 Clojure 中实现这样的功能。

# 如何进行匹配

要从字符串中删除特定模式的字符，我们可以使用 Clojure 的 `remove` 函数。这个函数接受两个参数，第一个是谓词函数，用于定义匹配条件；第二个是要进行匹配操作的字符串。让我们来看一个示例：

```Clojure
(remove #{\s} "Hello World")
```

上述代码将使用 `remove` 函数来删除字符串中的空格字符。我们可以在第一个参数中定义任何匹配条件，只要符合这个条件的字符都会被删除。让我们来看一下输出结果：

```Clojure
("HelloWorld")
```

如你所见，空格字符已经被成功删除了。值得注意的是，这里使用了 Clojure 的 `#{}` 语法来定义一个集合，其中包含要删除的字符。除了单个字符，我们也可以使用正则表达式来匹配更复杂的模式。

# 深入探讨

除了使用 `remove` 函数外，我们还可以使用 `filter` 函数来实现删除字符的功能。这个函数也接受一个谓词函数作为第一个参数，但它会返回匹配的字符，而不是删除不匹配的字符。因此，我们需要使用 `apply` 函数来组合 `filter` 函数和 `str` 函数来得到最终的结果。让我们看一个例子：

```Clojure
(apply str (filter #{\l \d} "Hello World 123"))
```

上述代码中，我们先使用 `filter` 函数和一个包含 `l` 和 `d` 的集合来过滤字符串中的字符。然后再使用 `apply` 函数和 `str` 函数将过滤后的结果组合成一个字符串。让我们来看一下输出：

```Clojure
("lloorld123")
```

如你所见，除了匹配的字符，其他字符都被删除了。

# 参考链接

- [Clojure Docs - remove](https://clojuredocs.org/clojure.core/remove)
- [Clojure Docs - filter](https://clojuredocs.org/clojure.core/filter)
- [Clojure Docs - apply](https://clojuredocs.org/clojure.core/apply)
- [Regular Expressions in Clojure](https://clojure.org/guides/learn/regular_expressions)