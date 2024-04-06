---
date: 2024-01-20 17:50:47.582103-07:00
description: "How to: \u5982\u4F55\u64CD\u4F5C\uFF1A Clojure \u5E76\u6CA1\u6709\u5185\
  \u7F6E\u5B57\u7B26\u4E32\u63D2\u503C\uFF0C\u9700\u8981\u901A\u8FC7\u5176\u5B83\u65B9\
  \u5F0F\u5982 `str` \u6216 `format` \u51FD\u6570\u6765\u62FC\u63A5\u3002\u8FD9\u79CD\
  \u505A\u6CD5\u6765\u6E90\u4E8E Lisp \u7684\u4F20\u7EDF\uFF0C\u5728\u8FD9\u4E2A\u4F20\
  \u7EDF\u4E2D\uFF0C\u7B80\u5355\u548C\u76F4\u63A5\u5E38\u5E38\u80DC\u4E8E\u8BED\u6CD5\
  \u7CD6\u3002\u4F60\u8FD8\u53EF\u4EE5\u7528 `clojure.pprint/cl-format` \u6765\u83B7\
  \u5F97\u66F4\u591A\u7C7B\u4F3C `printf`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.639432-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Clojure \u5E76\u6CA1\u6709\u5185\u7F6E\u5B57\
  \u7B26\u4E32\u63D2\u503C\uFF0C\u9700\u8981\u901A\u8FC7\u5176\u5B83\u65B9\u5F0F\u5982\
  \ `str` \u6216 `format` \u51FD\u6570\u6765\u62FC\u63A5\u3002\u8FD9\u79CD\u505A\u6CD5\
  \u6765\u6E90\u4E8E Lisp \u7684\u4F20\u7EDF\uFF0C\u5728\u8FD9\u4E2A\u4F20\u7EDF\u4E2D\
  \uFF0C\u7B80\u5355\u548C\u76F4\u63A5\u5E38\u5E38\u80DC\u4E8E\u8BED\u6CD5\u7CD6\u3002\
  \u4F60\u8FD8\u53EF\u4EE5\u7528 `clojure.pprint/cl-format` \u6765\u83B7\u5F97\u66F4\
  \u591A\u7C7B\u4F3C `printf` \u7684\u529F\u80FD\uFF0C\u8FD9\u4ECE Common Lisp \u7EE7\
  \u627F\u8FC7\u6765\u3002\u5BF9\u4E8E\u66F4\u591A\u73B0\u4EE3\u548C\u65B9\u4FBF\u7684\
  \u64CD\u4F5C\uFF0C\u53EF\u4EE5\u770B\u770B `clojure.string` \u5E93\uFF0C\u6216\u8005\
  \u4F7F\u7528\u8BF8\u5982 `hiccup` \u6216 `selmer` \u8FD9\u6837\u7684\u7B2C\u4E09\
  \u65B9\u6A21\u677F\u5E93\u3002"
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

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
