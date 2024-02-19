---
aliases:
- /zh/clojure/concatenating-strings/
date: 2024-01-20 17:34:35.178711-07:00
description: "\u5B57\u7B26\u4E32\u8FDE\u63A5\u5C31\u662F\u628A\u51E0\u4E2A\u5B57\u7B26\
  \u4E32\u62FC\u63A5\u6210\u4E00\u4E2A\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\
  \u8981\u662F\u4E3A\u4E86\u683C\u5F0F\u5316\u8F93\u51FA\u6216\u8005\u6784\u9020\u6570\
  \u636E\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:58.821180
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u8FDE\u63A5\u5C31\u662F\u628A\u51E0\u4E2A\u5B57\u7B26\
  \u4E32\u62FC\u63A5\u6210\u4E00\u4E2A\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\
  \u8981\u662F\u4E3A\u4E86\u683C\u5F0F\u5316\u8F93\u51FA\u6216\u8005\u6784\u9020\u6570\
  \u636E\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么?
字符串连接就是把几个字符串拼接成一个。程序员这么做主要是为了格式化输出或者构造数据。

## How to: 怎么做
在Clojure中，可以使用 `str` 函数来连接字符串。下面是几个例子。

```Clojure
(str "Hello, " "world!") ; 结果: "Hello, world!"

(let [name "Mandarin"]
  (str "Hello, " name " readers!")) ; 结果: "Hello, Mandarin readers!"
```

是不是挺简单的？

## Deep Dive 深入了解
早期编程语言如C用`strcat`来连接字符串，但那要处理指针和内存。Clojure作为现代Lisp方言, 设计得更高级，直接用`str`就行，简单多了，内部会替你处理好一切。

除了`str`，还有`clojure.string/join`，当你要把字符串集合拼接成一个字符串时，尤其有用。比如：

```Clojure
(clojure.string/join ", " ["apple" "banana" "cherry"]) ; 结果: "apple, banana, cherry"
```

实现上，`str`和`join`都要考虑性能因素。在背后，Clojure尽量利用Java字符串的不变性和StringBuffer/StringBuilder效率，让操作高效。

## See Also 查阅其他信息
- Clojure官网上的[字符串API文档](https://clojure.github.io/clojure/clojure.string-api.html)。
- 《Programming Clojure》一书中详细介绍了字符串操作。
- [ClojureDocs](https://clojuredocs.org/)提供了很多示例和文档。
