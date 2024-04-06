---
date: 2024-01-20 17:45:14.089019-07:00
description: "How to: \u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u8FD9\u4E2A\u64CD\u4F5C\
  \u5176\u5B9E\u6709\u5F88\u957F\u7684\u5386\u53F2\u3002\u5927\u591A\u8BED\u8A00\u63D0\
  \u4F9B\u8FD9\u529F\u80FD\u3002\u5728Clojure\u4E2D\uFF0C`subs`\u662F\u57FA\u4E8E\
  Java\u7684`substring`\uFF0C\u8FD9\u4FDD\u8BC1\u4E86\u9AD8\u6027\u80FD\u3002\u5B9E\
  \u9645\u4E0A\uFF0C\u5F53\u4F60\u7528`subs`\u65F6\uFF0CClojure\u5E95\u5C42\u76F4\u63A5\
  \u8C03\u7528\u4E86Java\u7684\u65B9\u6CD5\u3002 \u5176\u4ED6\u9009\u62E9\uFF0C\u6BD4\
  \u5982\u7528\u6B63\u5219\u8868\u8FBE\u5F0F\u505A\u66F4\u590D\u6742\u7684\u63D0\u53D6\
  \uFF0C\u4E5F\u662F\u53EF\u884C\u7684\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.518617-06:00'
model: gpt-4-1106-preview
summary: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u8FD9\u4E2A\u64CD\u4F5C\u5176\u5B9E\
  \u6709\u5F88\u957F\u7684\u5386\u53F2\u3002\u5927\u591A\u8BED\u8A00\u63D0\u4F9B\u8FD9\
  \u529F\u80FD\u3002\u5728Clojure\u4E2D\uFF0C`subs`\u662F\u57FA\u4E8EJava\u7684`substring`\uFF0C\
  \u8FD9\u4FDD\u8BC1\u4E86\u9AD8\u6027\u80FD\u3002\u5B9E\u9645\u4E0A\uFF0C\u5F53\u4F60\
  \u7528`subs`\u65F6\uFF0CClojure\u5E95\u5C42\u76F4\u63A5\u8C03\u7528\u4E86Java\u7684\
  \u65B9\u6CD5\u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

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
