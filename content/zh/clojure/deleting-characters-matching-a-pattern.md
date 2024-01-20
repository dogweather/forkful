---
title:                "删除匹配模式的字符"
html_title:           "Java: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 什么与为什么？
模式匹配字符的删除是指通过编程从字符串中删除特定模式的字符。编程人员会这么做是因为这样可以高效地筛选和净化数据。

## 如何做：
在 Clojure 中有两种主要方法来删除字符串中匹配的字符：‘filter’函数和‘replace’函数。

```Clojure 
; 使用 filter 函数删除 ‘a’ 字符
(apply str (filter #(not= % \a) "banana"))
; 输出: "bnn"

; 使用 replace 函数删除所有 ‘a’ 字符
(clojure.string/replace "banana" "a" "")
; 输出: "bnn"
``` 

## 深入探究
‘filter’函数是从 Clojure 得基函数库中派生出来的，并且通常用于更大范围的‘筛选’任务，不仅限于字符串处理。相比之下，‘replace’函数则更为专注于字符串操作，并且在处理大型字符串或者执行复杂的替换操作时可能会更有效率。

虽然删除匹配的字符似乎很直接，但实际上，它涉及到一些复杂的问题，如编码和正则表达式。在一些特定的边界情况中，开发者通常需要格外小心，否则可能会出现意想不到的结果。例如，你不能轻易地删除一个包含格式化符号的字符串（如“\n”或“\t”）中的反斜杠。

另外，还有一些其他的方法可以实现这个功能，例如使用‘remove’函数或者通过JAVA的字符串处理方法来达成。不过，‘filter’和‘replace’函数通常来说是更推荐的方式，因为它们可以提供更好的可读性和运行效率。

## 延伸阅读
Clojure官方文档提供了更多关于字符串操作的广泛知识和详细信息：[clojure.string documentation](https://clojure.github.io/clojure/clojure.string-api.html)

‘Regular Expressions’，或者称"'regex'，是一种用于字符串处理的强大工具。你可以在这里找到详细的信息和使用教程：[Regular Expressions Info](https://www.regular-expressions.info/tutorial.html). Backslashes 和 escape sequences 在字符串操作中也经常使用，更多信息可以在这里找到：[Java Documentation – Escape Sequences](https://docs.oracle.com/javase/tutorial/java/data/characters.html)