---
title:    "Clojure: 请用正则表达式"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 为什么使用正则表达式？

正则表达式是一种强大的工具，可以帮助程序员处理字符串数据。它们可以让我们更灵活地搜索和替换文本，并且可以帮助我们有效地提取所需的信息。使用正则表达式可以帮助节省时间和精力，并且提高我们的代码的可读性和可维护性。

# 如何使用正则表达式

使用正则表达式的第一步是导入`clojure.string`库，它包含了Clojure的正则表达式函数。然后，我们可以使用`re-find`函数来搜索字符串中的某个模式，并返回匹配的结果。例如，我们想要提取字符串中的所有数字，我们可以使用以下代码：

```Clojure
(def text "I have 3 apples and 5 bananas.")

(re-find #"\d+" text)

;; 输出： "3"
```

我们也可以使用`re-seq`函数来返回所有匹配的结果作为一个列表。例如，我们想要提取字符串中所有的单词，可以使用以下代码：

```Clojure
(def text "I have 3 apples and 5 bananas.")

(re-seq #"[a-zA-Z]+" text)

;; 输出： ["I" "have" "apples" "and" "bananas"]
```

除了`re-find`和`re-seq`，Clojure还提供了许多其他有用的正则表达式函数，如`re-matches`，`subs`和`replace-first`。我们可以在[官方文档](https://clojuredocs.org/clojure.string/re-matches)中找到更多函数的使用方法和示例。

# 深入了解正则表达式

正则表达式的语法相对复杂，但是一旦我们掌握了它，就可以大大提高我们处理字符串的能力。例如，使用元字符可以匹配特定的字符类型，如`\d`代表数字，`\s`代表空白字符，`\w`代表字母或数字等。我们也可以使用锚点来限制匹配的位置，如`^`代表开头，`$`代表结尾。在深入学习正则表达式时，我们也可以学习一些高级概念，如捕获和分组以及正则表达式的可视化工具。

# 另请参阅

- [正则表达式教程](https://www.runoob.com/regexp/regexp-tutorial.html)
- [Mastering Regular Expressions](https://regex.info/book.html) by Jeffrey Friedl
- [正则表达式入门：5分钟快速学习](https://www.zhihu.com/question/22061880)
- [Clojure官方文档](https://clojuredocs.org/)