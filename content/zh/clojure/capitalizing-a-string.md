---
title:                "将字符串大写"
html_title:           "Clojure: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么与为什么？
字符串大写化指的是将字符串中的所有字母都变为大写。程序员之所以要做这个，是因为在许多情况下（比如创建标识符或进行大小写无关的比较时），大写字符的一致性和可预测性很重要。

## 如何做：
在Clojure中，将字符串大写的标准函数是`clojure.string/upper-case`。它接受一个字符串参数并返回一个新的全大写字符串。

```Clojure
(require '[clojure.string :as str])

(str/upper-case "hello world")
; 输出: "HELLO WORLD"
```

## 深入探索
字符串大写化最初在ASCII字符集中实现，包括A至Z的26个大写字母和a至z的26个小写字母。在现代编程语言中，包括Clojure，字符串大写化已经被扩展到包含Unicode字符。

有些时候，你可能会遇到一些需要在特定条件下大写化字符串的情况。例如，当你需要将第一个字母大写化而保持其余字母小写（比如标题或人名）时，你可以使用`clojure.string/capitalize`函数。

```Clojure
(require '[clojure.string :as str])

(str/capitalize "hello world")
; 输出: "Hello world"
```

## 参考资料
以下是一些其他有关在Clojure中操作字符串的资源：

- Clojure官方文档中的[clojure.string函数库](https://clojure.github.io/clojure/clojure.string-api.html)
- Clojuredocs中的相关文章，例如[关于clojure.string/upper-case的详细解释](https://clojuredocs.org/clojure.string/upper-case)和[关于clojure.string/capitalize的详细解释](https://clojuredocs.org/clojure.string/capitalize)
- [Clojure编程的基础知识](https://www.braveclojure.com/clojure-for-the-brave-and-true/)，它包含了处理字符串的实用教程一部分。