---
title:                "搜索和替换文本"
date:                  2024-01-20T17:57:35.676020-07:00
model:                 gpt-4-1106-preview
simple_title:         "搜索和替换文本"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/searching-and-replacing-text.md"
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
