---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:52.314383-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u4F5C\u4E3A\u4E00\u4E2AJVM\u8BED\u8A00\uFF0C\
  Clojure\u5141\u8BB8\u4F60\u76F4\u63A5\u4F7F\u7528Java\u7684\u65E5\u671F\u548C\u65F6\
  \u95F4\u5E93\u3002\u8BA9\u6211\u4EEC\u9996\u5148\u5F00\u59CB\u4F7F\u7528\u5185\u7F6E\
  \u7684Java\u4E92\u64CD\u4F5C\u6027\uFF0C\u7136\u540E\u63A2\u7D22\u5982\u4F55\u4F7F\
  \u7528\u4E00\u4E2A\u53D7\u6B22\u8FCE\u7684\u7B2C\u4E09\u65B9\u5E93clj-time\uFF0C\
  \u4EE5\u66F4\u7B26\u5408Clojure\u4E60\u60EF\u7684\u65B9\u5F0F\u89E3\u51B3\u95EE\u9898\
  \u3002"
lastmod: '2024-04-05T21:53:47.665388-06:00'
model: gpt-4-0125-preview
summary: "\u4F5C\u4E3A\u4E00\u4E2AJVM\u8BED\u8A00\uFF0CClojure\u5141\u8BB8\u4F60\u76F4\
  \u63A5\u4F7F\u7528Java\u7684\u65E5\u671F\u548C\u65F6\u95F4\u5E93\u3002\u8BA9\u6211\
  \u4EEC\u9996\u5148\u5F00\u59CB\u4F7F\u7528\u5185\u7F6E\u7684Java\u4E92\u64CD\u4F5C\
  \u6027\uFF0C\u7136\u540E\u63A2\u7D22\u5982\u4F55\u4F7F\u7528\u4E00\u4E2A\u53D7\u6B22\
  \u8FCE\u7684\u7B2C\u4E09\u65B9\u5E93clj-time\uFF0C\u4EE5\u66F4\u7B26\u5408Clojure\u4E60\
  \u60EF\u7684\u65B9\u5F0F\u89E3\u51B3\u95EE\u9898\u3002"
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
