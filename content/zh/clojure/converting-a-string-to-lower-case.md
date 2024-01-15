---
title:                "将字符串转换为小写"
html_title:           "Clojure: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

当处理文字数据时，有时候需要将字符串转换成小写字母。这可以使得文本数据更具统一性和可读性，在处理文本匹配和比较时也会更方便。

## 怎么做

要将字符串转换成小写，我们可以使用Clojure内置的`lower-case`函数。它接受一个字符串作为参数，并返回一个全部为小写的新字符串。举个例子：

```Clojure
; 定义一个字符串
(def s "HELLO WORLD")

; 使用lower-case函数
(lower-case s)
```

这样，输出就会是`"hello world"`。如果我们有多个字符串需要转换，可以使用Clojure的`map`函数来批量处理：

```Clojure
; 定义多个字符串
(def str1 "APPLE")
(def str2 "BANANA")
(def str3 "CARROT")

; 使用map函数将全部转换成小写
(map lower-case [str1 str2 str3])

; 输出结果为：("apple" "banana" "carrot")
```

另外，我们也可以使用`into`函数将转换后的字符串存储到一个新的数据结构中，如列表或向量：

```Clojure
; 将转换后的字符串存储到列表中
(into [] (map lower-case ["HELLO" "WORLD"]))

; 输出结果为：["hello" "world"]

; 将转换后的字符串存储到向量中
(into [] (map lower-case ["HELLO" "WORLD"]))

; 输出结果为：["hello" "world"]
```

使用`lower-case`函数还可以处理中文字符串，它会将中文字符转换成小写拼音，例如：

```Clojure
; 将中文字符串转换成小写
(lower-case "你好世界")

; 输出结果为："nihao shijie"
```

## 深入探讨

在Clojure中，字符串是不可变的数据类型。这意味着当我们使用`lower-case`函数对一个字符串进行转换时，它并不会改变原始字符串，而是返回一个新的字符串。这种不可变性的设计有助于保证程序的稳定性和线程安全性。

此外，Clojure也提供了其他字符串转换函数，如`upper-case`、`capitalize`和`clojure.string/lower-case`等，可以根据具体需求来使用。

## 参考资料

- [Clojure官方文档 - 字符串函数](https://clojure.org/reference/strings)
- [Clojure学习资源汇总](https://github.com/learn-by-doing/clojure-resources)

## 参考

[Why Clojure is Awesome](https://clojure.org/about/advantages)