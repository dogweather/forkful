---
title:                "使用正则表达式"
html_title:           "Arduino: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 什么和为什么？

正则表达式是一种强大的字符串处理工具，允许我们匹配、查找和替换复杂的文本模式。编程人员使用它们来完成处理文件、验证输入等任务。

## 如何？：

在Clojure中，我们使用 `re-pattern`、`re-matches` 和 `re-seq` 用于正则表达式。`re-pattern` 用于编译正则表达式，`re-matches` 用于完全匹配，`re-seq` 用于部分匹配。

```clojure
(def re (re-pattern "\\d+")) ;匹配一个或多个数字
(re-matches re "1234") ;=> "1234"
(re-seq re "abc 1234 xyz 5678") ;=> ("1234" "5678")
```

## 深入研究： 

1. 历史背景：正则表达式最早在20世纪60年代的Unix工具中出现，作为文本处理的实用工具。
2. 替代方案：虽然正则表达式非常强大，但并不总是最佳解决方案。有时，简单的字符串操作或解析库可能会更易于使用和理解。
3. 实现细节：在Clojure中，正则表达式实现是由Java的 `java.util.regex` 类提供的。这意味着Clojure的正则表达式有着和Java完全一样的特性和语法。

## 另请参阅：

- Java的 [`Pattern`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/regex/Pattern.html) 类文档，可以详细了解其正则表达式语法和特性。