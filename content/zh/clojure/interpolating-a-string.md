---
title:                "字符串插值"
date:                  2024-01-20T17:50:47.582103-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串插值"

category:             "Clojure"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 什么 & 为什么？
字符串插值是在字符串中嵌入变量或表达式的过程。程序员这么做是为了构造动态内容，提高代码的灵活性和可读性。

## How to: 如何操作：
```Clojure
;; 使用 str 连接字符串和变量值
(let [name "World"]
  (str "Hello, " name "!"))
;; 输出: "Hello, World!"

;; 使用 format 进行更复杂的插值
(let [user "Jane" age 28]
  (format "Hi, my name is %s and I am %d years old." user age))
;; 输出: "Hi, my name is Jane and I am 28 years old."
```

## Deep Dive 深入探索
Clojure 并没有内置字符串插值，需要通过其它方式如 `str` 或 `format` 函数来拼接。这种做法来源于 Lisp 的传统，在这个传统中，简单和直接常常胜于语法糖。你还可以用 `clojure.pprint/cl-format` 来获得更多类似 `printf` 的功能，这从 Common Lisp 继承过来。对于更多现代和方便的操作，可以看看 `clojure.string` 库，或者使用诸如 `hiccup` 或 `selmer` 这样的第三方模板库。

## See Also 另请参阅
- Clojure 官方文档：[Clojure API Docs](https://clojure.github.io/clojure/)
- `clojure.string` 文档：[Clojure Strings](https://clojure.github.io/clojure/clojure.string-api.html)
- 第三方库如 Selmer：[Selmer Template Library](https://github.com/yogthos/Selmer)
