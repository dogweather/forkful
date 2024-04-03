---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:52.314383-07:00
description: "\u5728Clojure\u4E2D\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\
  \u662F\u5C06\u65E5\u671F\u548C\u65F6\u95F4\u7684\u6587\u672C\u8868\u73B0\u5F62\u5F0F\
  \u8F6C\u6362\u6210\u66F4\u52A0\u53EF\u7528\u7684\u5F62\u6001\uFF08\u4F8B\u5982\uFF0C\
  Clojure\u7684DateTime\u5BF9\u8C61\uFF09\u3002\u8FD9\u4E2A\u8FC7\u7A0B\u5BF9\u4E8E\
  \u6570\u636E\u5904\u7406\u3001\u65E5\u5FD7\u8BB0\u5F55\u6216\u4EFB\u4F55\u64CD\u4F5C\
  \u65F6\u95F4\u6570\u636E\u7684\u5E94\u7528\u7A0B\u5E8F\u6765\u8BF4\u662F\u57FA\u7840\
  \uFF0C\u4F7F\u5F97\u7A0B\u5E8F\u5458\u80FD\u591F\u6709\u6548\u5730\u6267\u884C\u65E5\
  \u671F\u7684\u64CD\u4F5C\u3001\u6BD4\u8F83\u6216\u5904\u7406\u4EFB\u52A1\u3002"
lastmod: '2024-03-13T22:44:47.313510-06:00'
model: gpt-4-0125-preview
summary: "\u5728Clojure\u4E2D\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\
  \u662F\u5C06\u65E5\u671F\u548C\u65F6\u95F4\u7684\u6587\u672C\u8868\u73B0\u5F62\u5F0F\
  \u8F6C\u6362\u6210\u66F4\u52A0\u53EF\u7528\u7684\u5F62\u6001\uFF08\u4F8B\u5982\uFF0C\
  Clojure\u7684DateTime\u5BF9\u8C61\uFF09\u3002\u8FD9\u4E2A\u8FC7\u7A0B\u5BF9\u4E8E\
  \u6570\u636E\u5904\u7406\u3001\u65E5\u5FD7\u8BB0\u5F55\u6216\u4EFB\u4F55\u64CD\u4F5C\
  \u65F6\u95F4\u6570\u636E\u7684\u5E94\u7528\u7A0B\u5E8F\u6765\u8BF4\u662F\u57FA\u7840\
  \uFF0C\u4F7F\u5F97\u7A0B\u5E8F\u5458\u80FD\u591F\u6709\u6548\u5730\u6267\u884C\u65E5\
  \u671F\u7684\u64CD\u4F5C\u3001\u6BD4\u8F83\u6216\u5904\u7406\u4EFB\u52A1\u3002."
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

## 如何操作:
作为一个JVM语言，Clojure允许你直接使用Java的日期和时间库。让我们首先开始使用内置的Java互操作性，然后探索如何使用一个受欢迎的第三方库clj-time，以更符合Clojure习惯的方式解决问题。

### 使用Java互操作
Clojure可以直接利用Java的`java.time.LocalDate`来从字符串解析日期：
```clojure
(require '[clojure.java.io :as io])

; 使用Java互操作解析日期
(let [date-str "2023-04-01"
      date (java.time.LocalDate/parse date-str)]
  (println date))
; 输出：2023-04-01
```

### 使用clj-time
一个更符合Clojure习惯用法的处理日期和时间的库是`clj-time`。它封装了Joda-Time，一个全面的日期和时间操作库。首先，你需要将`clj-time`添加到项目的依赖项中。以下是使用`clj-time`解析日期字符串的方法：

```clojure
; 请确保在你的project.clj的:dependencies下添加[clj-time "0.15.2"]

(require '[clj-time.format :as fmt]
         '[clj-time.core :as time])

; 定义一个格式化器
(let [formatter (fmt/formatter "yyyy-MM-dd")
      date-str "2023-04-01"
      parsed-date (fmt/parse formatter date-str)]
  (println parsed-date))
; 输出：#object[org.joda.time.DateTime 0x76eccb5d "2023-04-01T00:00:00.000Z"]
```

这些示例展示了基本的日期解析。这两种方法都很有用，但`clj-time`可以提供更符合Clojure方式的解决方案，并为复杂需求提供额外的功能。
