---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么和为什么？
字符串拼接是将两个或多个字符串组合在一起的过程。程序员通常这样做以创建用户要看到的特定文本或将数据组合以获取新的，更有用的字符串。

## 如何做：
Clojure 提供 `str` 函数来拼接字符串。请看以下简单示例：

```Clojure
(str "Hello" ", " "world!")
```

这将输出：

```Clojure
"Hello, world!"
```

你也可以拼接变量：

```Clojure
(def name "Alice")
(str "Hello, " name)
```

输出：

```Clojure
"Hello, Alice"
```

它甚至处理非字符串类型：

```Clojure
(str "The answer is: " 42)
```

输出：

```Clojure
"The answer is: 42"
```

## 深入探讨
Clojure 在 2007 年由 Rich Hickey 开发，main focus 是功能编程和可变状态的思想。尽管 `str` 函数使字符串拼接变得简单，但其历史可以追溯到 Lisp，Clojure 的前身。

另一种拼接字符串的方法是使用 `format` 函数，这稍微复杂一些，但提供更多格式化选项：

```Clojure
(format "Hello, %s" "world!")
```

此代码将输出 `"Hello, world!"`。`%s` 是占位符，它告诉 `format` 函数将字符串 `"world!"` 插入到该位置。

关于给字符串拼接分配内存的详细信息，Clojure 运行时会处理大部分情况，你通常不需要关心。但是，当处理大量数据时，可能有必要了解一些性能注意事项。如果你在疑问这是一个问题，请查阅相关文档以获得更多详细信息。

## 参考链接
- [Clojure 字符串函数](https://clojuredocs.org/clojure.string)
- [Clojure `format` 函数](https://clojuredocs.org/clojure.core/format)
- [关于 Lisp 和字符串的历史](http://www.paulgraham.com/lisp.html)