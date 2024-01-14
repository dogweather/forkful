---
title:                "Clojure: 提取子字符串"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么？

在Clojure编程中，我们经常需要从一个字符串中提取一个小片段，这也被称为提取子字符串。这种操作非常有用，因为它允许我们从大量的数据中获取我们需要的部分。无论是处理文本还是数据处理，提取子字符串都是一个常见的任务。

# 如何？

我们可以使用Clojure内置的`subs`函数来提取子字符串。让我们看一个例子：

```
Clojure
(let [str "这是一个例子字符串"]
  (subs str 4 6))
```

这个例子中，我们首先定义了一个字符串变量`str`，然后使用`subs`函数从第4个字符开始提取2个字符。在这个例子中，输出将是`例子`。

我们也可以通过使用`:end`关键字来指定最后一个字符的位置，例如：

```
Clojure
(let [str "这也是一个例子字符串"]
  (subs str 3 :end))
```

这个例子中，我们从第3个字符开始提取到字符串的末尾，输出将是`也是一个例子字符串`。

# 深入探讨

除了使用常规的`subs`函数外，Clojure还提供了更多功能丰富的函数来提取子字符串，例如`substring`和`clojure.string/substring`。这些函数提供了更多的选项来处理不同的情况，例如，使用负数来指定从字符串末尾开始提取。

此外，我们也可以使用正则表达式来提取子字符串，通过使用`re-find`和`re-matches`函数来匹配字符串并提取对应的子字符串。

# 请参阅

- [Clojure字符串函数文档](https://clojuredocs.org/clojure.string)
- [正则表达式在Clojure中的使用](https://clojuredocs.org/clojure.core/re-find)