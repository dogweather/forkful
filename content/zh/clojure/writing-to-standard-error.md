---
title:                "Clojure: 写入标准错误。"
simple_title:         "写入标准错误。"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 为什么

写入标准错误流是一种常见的编程技术，以在代码运行时输出错误信息。它可以帮助开发者在调试中快速定位错误，并提供更好的代码可读性。

# 如何

在Clojure中，我们可以使用`System/err`函数来访问标准错误流。下面是一个例子：

```Clojure
(System/err "这是一个错误信息")
```
这将在控制台中输出`这是一个错误信息`。我们也可以使用`println`函数来将错误信息打印到标准错误流中：

```Clojure
(println "这是一个错误信息" (System/err))
```

# 深入探讨

除了简单地输出错误信息外，我们还可以使用`with-out-str`函数来将错误信息以字符串的形式存储起来，以便后续使用。下面是一个例子：

```Clojure
(with-out-str
  (System/err "这是一个错误信息"))
```

此时，错误信息将被存储在一个字符串中，我们可以用`println`函数来输出它：

```Clojure
(println "错误信息: " (with-out-str
                        (System/err "这是一个错误信息")))
```

这将在控制台中输出`错误信息: 这是一个错误信息`。

# 参考资料

- [Clojure官方文档](https://clojure.org/api/cheatsheet)
- [错误处理 - CLojure教程](https://www.w3cschool.cn/clojure/clojure-error-handling.html)

# 参见

- [Clojure常用编程技巧](https://blog.csdn.net/vic_lee/article/details/79963486)
- [错误处理 - Clojure学习手册](https://clojure.org/guides/learn/error_handling)