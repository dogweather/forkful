---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
将字符串转换为小写就是把字符串的所有字母都变为小写。程序员这样做主要是为了便于文本比较和搜索。

## 如何操作：
Clojure 提供了易于使用的函数 `clojure.string/lower-case` 来将字符串转换为小写。看看下面的例子：

```clojure
(ns example.core
  (:require [clojure.string :as str]))

(defn -main
  [& args]
  (println (str/lower-case "Hello, World!")))
```
运行这段程序，将打印：
```clojure
hello, world!
```

## 深入挖掘
在早期的计算机系统中，大写字符和小写字符被认为是完全不同的字符，这会导致混乱和不一致。所以，
程序员发明了组编程语言的方法来统一字符，其中一种就是将字符串转换为小写。早期的 Lisp 语言就没有这个功能，但后来 Clojure，作为一种现代化的 Lisp 方言，加入了这个功能。这样做可以减少错误并提高程序的鲁棒性。

你也可以创建自定义函数来对字符串的每个字符进行操作。但是使用 `clojure.string/lower-case` 通常更加简单且高效。

关于在 Clojure 中如何将字符串转为小写的更多细节，`clojure.string/lower-case` 函数在内部使用 `java.lang.String.toLowerCase()`，它考虑到了国际化和特殊字符。

## 另请参阅
* Clojure 官方文档关于 `clojure.string/lower-case` 的描述：https://clojuredocs.org/clojure.string/lower-case
* Oracle 对 `java.lang.String.toLowerCase()` 的文档：https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--