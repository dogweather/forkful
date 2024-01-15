---
title:                "字符串转换为大写"
html_title:           "Clojure: 字符串转换为大写"
simple_title:         "字符串转换为大写"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么
你可能会想知道，在进行字符串大写处理时存在哪些优势。其实，字符串大写处理是一种常见的格式化操作，它可以让你的代码更易读，并遵循编程规范。

## 如何进行字符串大写处理
如果你使用的是Clojure语言，进行字符串大写处理非常简单。你可以使用内置函数 `clojure.string/upper-case`来实现，它可以接收一个字符串参数并返回其大写形式。

```Clojure
(clojure.string/upper-case "hello world") ; 输出 "HELLO WORLD"
```

如果你想对多个字符串进行大写处理，可以使用 `map` 函数来遍历一个字符串列表，并应用 `clojure.string/upper-case` 函数。下面的例子演示了如何将一个字符串列表中的所有元素转换为大写形式。

```Clojure
(map clojure.string/upper-case ["foo" "bar" "baz"]) ; 输出 ("FOO" "BAR" "BAZ")
```

## 深入了解字符串大写处理
除了使用内置函数 `clojure.string/upper-case`，你也可以使用 `String/toUpperCase` 方法来实现字符串的大写处理。这个方法是Java提供的，Clojure可以无缝地使用它。下面的代码展示了如何使用 `String/toUpperCase` 方法来将一个字符串转换为大写形式。

```Clojure
(.toUpperCase "hello world") ; 输出 "HELLO WORLD"
```

此外，你也可以使用正则表达式来进行字符串的大写处理。通过使用 `re-seq` 函数，你可以将字符串拆分为一个个字符，然后使用 `Character/toUpperCase` 方法来将每个字符转换为大写形式。下面的例子展示了如何通过正则表达式来实现字符串的大写处理。

```Clojure
(map (fn [char] (Character/toUpperCase (str char))) (re-seq #".*" "hello world")) ; 输出 ("H" "E" "L" "L" "O" " " "W" "O" "R" "L" "D")
```

## 更多相关资源
- [Clojure语言官方网站](https://clojure.org/)
- [Clojure从零开始学习指南](https://github.com/StrangeAptos/clojure-from-zero)
- [Clojure字符串处理库](https://github.com/clojure/data.string) 

# 参考链接
- [Clojure String 大写处理](https://weishan.me/clojure-string-upper-2/)