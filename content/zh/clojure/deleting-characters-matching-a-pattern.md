---
title:                "匹配模式删除字符"
date:                  2024-01-20T17:42:19.107995-07:00
model:                 gpt-4-1106-preview
simple_title:         "匹配模式删除字符"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
删除匹配模式的字符意味着找出符合特定规则的字符并将它们移除。程序员这么做来清理数据，比如去掉无用的标点或空格，或者出于安全原因，比如移除可能导致代码注入的字符。

## How to: 如何操作：
Clojure中删除特定模式的字符很简单。用`clojure.string/replace`函数配合正则表达式来完成：

```Clojure
(require '[clojure.string :as str])

;; 删除所有数字
(defn remove-digits [s]
  (str/replace s #"\d+" ""))

(remove-digits "Ab3d5Ef6g")
;; 输出："AbdEfg"

;; 删除特定的字符，比如美元符号 and 百分号
(defn remove-specific-chars [s]
  (str/replace s #"\$|%" ""))

(remove-specific-chars "Price is $100 and 20% discount")
;; 输出："Price is 100 and 20 discount"
```

## Deep Dive: 深入探讨
`clojure.string/replace`函数是Clojure中处理字符串替换的重要工具。其背后使用的是Java的`java.lang.String`和`java.util.regex`包，这显示了Clojure与Java平台的良好兼容性。此功能自Clojure诞生以来一直存在，并且是处理文本任务时的常用手段。虽然Clojure内置了强大的正则表达式支持，但使用其他文本处理库，如`clojure.core.reducers`，也是可行的选择。另外，还可以借助Java的`java.text`包来实现更复杂的字符操作。

## See Also: 另请参阅
- Clojure官方文档关于`clojure.string/replace`的说明：[Clojure - clojure.string](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/replace)
- Java正则表达式指南：[Java Pattern Class](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
