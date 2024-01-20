---
title:                "搜索和替换文本"
html_title:           "Kotlin: 搜索和替换文本"
simple_title:         "搜索和替换文本"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 什么和为什么？ (What & Why?)

搜索和替换文字就是找到指定的字符串并换成新的字符串。程序员这么做是为了修改代码、数据和文本内容。

## 如何操作： (How to)

在Clojure中，我们使用 `clojure.string/replace` 函数来进行搜索和替换操作。 请看以下代码示例，

```Clojure
(require '[clojure.string :as str])

(str/replace "Hello, World!" "World" "Clojure")
```

运行上述代码，输出的结果会是：

```Clojure
"Hello, Clojure!"
```

在此，我们在“Hello, World!”这个字符串中搜索“World”并替换成了“Clojure”。

## 深入了解 (Deep Dive)

1. 历史背景：Clojure语言的设计哲学一直是提供强大简洁的核心库和丰富实用的函数。`clojure.string/replace`就是其中的佳作。
 
2. 替代方法：也可以使用Java的replace方法在Clojure中替换文本，比如`(.replace "Hello, World!" "World" "Clojure")`。

3. 执行细节：`clojure.string/replace`函数在内部使用Java的 `String.replace` 或 `Matcher.replaceAll`方法，取决于你是使用固定的字符串替换还是用正则表达式替换。

## 另请参阅 (See Also)

1. [Clojure官方对clojure.string库的文档](https://clojure.github.io/clojure/clojure.string-api.html)
2. [Regex表达式的示例和教程](https://www.regexr.com/)