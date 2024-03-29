---
date: 2024-01-20 17:45:14.089019-07:00
description: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5C31\u662F\u4ECE\u4E00\u4E2A\u66F4\
  \u957F\u7684\u5B57\u7B26\u4E32\u4E2D\u62FF\u51FA\u4E00\u90E8\u5206\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5904\u7406\u6216\u5206\u6790\u7279\u5B9A\
  \u7684\u6570\u636E\u7247\u6BB5\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.291124-06:00'
model: gpt-4-1106-preview
summary: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5C31\u662F\u4ECE\u4E00\u4E2A\u66F4\
  \u957F\u7684\u5B57\u7B26\u4E32\u4E2D\u62FF\u51FA\u4E00\u90E8\u5206\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5904\u7406\u6216\u5206\u6790\u7279\u5B9A\
  \u7684\u6570\u636E\u7247\u6BB5\u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## What & Why?
提取子字符串就是从一个更长的字符串中拿出一部分。程序员这么做是为了处理或分析特定的数据片段。

## How to:
Clojure里提取子字符串很直接。用`subs`函数就行:

```Clojure
;; 截取一个字符串的一部分
(def original-string "Clojure欢迎你")

;; 从索引2开始，提取到索引8（不包括8）
(def extracted-substring (subs original-string 2 8))

(println extracted-substring) ;; 输出: ojure欢
```

记住，索引是从0开始的。

## Deep Dive
提取子字符串这个操作其实有很长的历史。大多语言提供这功能。在Clojure中，`subs`是基于Java的`substring`，这保证了高性能。实际上，当你用`subs`时，Clojure底层直接调用了Java的方法。

其他选择，比如用正则表达式做更复杂的提取，也是可行的：

```Clojure
;; 使用正则表达式提取子字符串
(def original-string "Find #clojure in this string")
(re-find #"(?<=#)\w+" original-string) ;; 输出: "clojure"
```

提取子字符串时，尤其注意边界条件。比如下标越界，会产生错误。

## See Also
- Clojure `subs` 函数官方文档：[Clojure Subs](https://clojuredocs.org/clojure.core/subs)
- 更多关于正则表达式在Clojure中的用法：[Clojure Regex](https://clojuredocs.org/clojure.core/re-find)
- Java `substring` 方法介绍：[Java Substring](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int))
