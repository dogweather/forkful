---
date: 2024-01-20 17:57:35.676020-07:00
description: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u5C31\u662F\u627E\u51FA\u6307\
  \u5B9A\u7684\u5B57\u7B26\u4E32\u5E76\u7528\u5176\u4ED6\u5B57\u7B26\u4E32\u4EE3\u66FF\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u66F4\u65B0\u6570\u636E\
  \u3001\u4FEE\u6B63\u9519\u8BEF\u6216\u6539\u8FDB\u4EE3\u7801\u53EF\u8BFB\u6027\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.287113-06:00'
model: gpt-4-1106-preview
summary: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u5C31\u662F\u627E\u51FA\u6307\
  \u5B9A\u7684\u5B57\u7B26\u4E32\u5E76\u7528\u5176\u4ED6\u5B57\u7B26\u4E32\u4EE3\u66FF\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u66F4\u65B0\u6570\u636E\
  \u3001\u4FEE\u6B63\u9519\u8BEF\u6216\u6539\u8FDB\u4EE3\u7801\u53EF\u8BFB\u6027\u3002"
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

搜索和替换文本就是找出指定的字符串并用其他字符串代替。程序员这么做是为了更新数据、修正错误或改进代码可读性。

## How to: (怎么做：)

Clojure里用`clojure.string/replace`来搜索替换：

```clojure
(require '[clojure.string :as str])

;; 替换简单字符串
(str/replace "I love Clojure!" "love" "adore")
;; => "I adore Clojure!"

;; 使用正则表达式替换所有匹配项
(str/replace "E=mc^2 is quite famous in physics." #"[a-z]" "X")
;; => "E=XX^X XX XXXXX XXXXXX XX XXXXXXX."
```

## Deep Dive (深入探究)

搜索替换很早就在文本编辑和处理程序中出现，例如sed和awk。Clojure作为一门现代Lisp，提供了强大的字符串处理库。除了`clojure.string/replace`，还有其他函数，例如`re-seq`和`re-find`，它们可以帮助细致地处理文本。正则表达式支持也让搜索更加灵活。

搜索和替换在Clojure中通过Java的正则表达式引擎实现，性能很好。但要注意：替换操作产生新的字符串，原始字符串保持不变，这是因为Clojure中字符串是不可变的。

## See Also (另请参阅)

- Clojure官方文档的字符串处理部分：[Clojure Strings](https://clojure.org/guides/learn/functions#_strings)
- Oracle官方关于Java正则表达式的文档：[Java Regex](https://docs.oracle.com/javase/tutorial/essential/regex/)
- 在线正则表达式测试器：[Regexr](https://regexr.com/)
