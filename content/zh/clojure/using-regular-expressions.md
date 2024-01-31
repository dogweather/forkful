---
title:                "使用正则表达式"
date:                  2024-01-19
simple_title:         "使用正则表达式"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? | 什么是正则表达式，以及为什么要使用？

正则表达式是一种文本模式匹配的工具。程序员用它来搜索、编辑、检查或操纵字符串。

## How to: | 如何操作：

```Clojure
;; 匹配字符串
(re-find #"hello" "hello world")
; => "hello"

;; 分割字符串
(re-seq #"\d+" "The numbers are 42 and 1234")
; => ("42" "1234")

;; 替换文本
(clojure.string/replace "foo123" #"\d+" "ABC")
; => "fooABC"

;; 提取具体信息
(let [regex #"(?i)the (\w+) fox"]
  (re-find regex "The Quick Brown Fox"))
; => ["The Quick fox" "Quick"]
```

## Deep Dive | 深入了解：

正则表达式起源于上世纪50年代的理论计算机科学领域，现在是文本处理不可或缺的工具。Clojure作为一种现代的Lisp方言，直接在语言层面支持正则表达式。与Perl或Python等语言相比，Clojure中正则表达式的使用更简洁，因其采用了Java的正则表达式引擎。一些常见的替代方案包括字符串处理函数和parser combinator库，但它们经常不如正则表达式高效。

## See Also | 相关资源：

- Clojure官方文档关于正则表达式的章节：[Clojure - Regular Expressions](https://clojure.org/guides/learn/functions#_regular_expressions)
- Java正则表达式文档，了解Clojure背后的引擎：[Java Pattern Class](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- 正则表达式的详细入门指导：[Regular-Expressions.info](https://www.regular-expressions.info/)
