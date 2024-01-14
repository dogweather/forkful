---
title:                "Clojure: 大写字符串"
simple_title:         "大写字符串"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么要将字符串大写化

在编程中，有时需要将字符串的一部分或全部字符转换为大写。这对于格式化输出、比较字符串、或者是避免大小写敏感的问题都非常有用。Clojure提供了简单的方法来实现这一功能，让我们来深入了解一下如何实现吧！

## 如何进行字符串大写化

首先，我们需要使用Clojure内置的函数`clojure.string/upper-case`来将字符串转换为全部大写。这个函数需要一个字符串作为参数，并返回大写化后的字符串。下面是一个简单的例子代码：

```Clojure
(def str "hello world!")
(clojure.string/upper-case str)
```

输出结果将会是`"HELLO WORLD!"`。

如果我们想要将字符串的一部分转换为大写，可以使用`subs`函数来先截取需要转换的部分，再使用`upper-case`函数来进行转换。例如，将字符串的前三个字符转换为大写：

```Clojure
(def str "hello world!")
(clojure.string/upper-case (subs str 0 3))
```

输出结果将会是`"HEL"`。

另外，我们还可以使用`map`函数来对字符串中的每个字符进行转换，从而实现将整个字符串转换为大写的效果。例如，我们可以定义一个自定义函数来实现这个功能：

```Clojure
(defn custom-upper-case [s]
  (apply str (map clojure.string/upper-case s)))
(custom-upper-case "hello world!")
```

输出结果将会是`"HELLO WORLD!"`。

## 深入探究字符串大写化

在Clojure中，字符串是不可变的数据类型，这意味着我们无法直接更改字符串中的字符。所以，如果我们想要修改字符串中的某个字符为大写，需要先将字符串转换为可变的数据类型，比如列表，然后再修改其中的字符并转换回字符串。例如：

```Clojure
(def str "hello world!")
(apply str (assoc (into [] str) 0 (clojure.string/upper-case (subs str 0 1))))
```

输出结果将会是`"Hello world!"`。

此外，如果我们想要在字符串中的特定位置插入大写的字符，可以先使用`clojure.string/replace`函数来实现。例如，在字符串的第三个字符后插入大写的“X”：

```Clojure
(def str "hello world!")
(clojure.string/replace str #"(.{3})" "$1X")
```

输出结果将会是`"helXlo world!"`。

## 参考资料

- [Clojure Docs: clojure.string/upper-case](https://clojuredocs.org/clojure.string/upper-case)
- [Clojure Docs: clojure.string/replace](https://clojuredocs.org/clojure.string/replace)
- [Clojurists Together: How to capitalize and hash a string in Clojure](https://www.clojuriststogether.org/news/blog/2018/08/10/how-to-capitalize-and-hash-a-string-in-clojure/)
- [Clojure for the Brave and True: Strings](https://www.braveclojure.com/core-functions-in-depth/#Strings)

## 查看也许感兴趣

- [Clojure Docs: clojure.string/substr](https://clojuredocs.org/clojure.string/substr)
- [Clojure Docs: clojure.string/join](https://clojuredocs.org/clojure.string/join)
- [Clojurists Together: Parsing CSV in Clojure with clojure-csv](https://www.clojuriststogether.org/news/blog/2018/07/02/parsing-csv-in-clojure-with-clojure-csv/)