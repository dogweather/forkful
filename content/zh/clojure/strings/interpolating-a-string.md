---
date: 2024-01-20 17:50:47.582103-07:00
description: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u5728\u5B57\u7B26\u4E32\u4E2D\u5D4C\
  \u5165\u53D8\u91CF\u6216\u8868\u8FBE\u5F0F\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u6784\u9020\u52A8\u6001\u5185\u5BB9\uFF0C\u63D0\
  \u9AD8\u4EE3\u7801\u7684\u7075\u6D3B\u6027\u548C\u53EF\u8BFB\u6027\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.369524
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u5728\u5B57\u7B26\u4E32\u4E2D\u5D4C\
  \u5165\u53D8\u91CF\u6216\u8868\u8FBE\u5F0F\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u6784\u9020\u52A8\u6001\u5185\u5BB9\uFF0C\u63D0\
  \u9AD8\u4EE3\u7801\u7684\u7075\u6D3B\u6027\u548C\u53EF\u8BFB\u6027\u3002"
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
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
