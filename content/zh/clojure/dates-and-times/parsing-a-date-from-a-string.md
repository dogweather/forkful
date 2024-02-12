---
title:                "从字符串解析日期"
aliases:
- /zh/clojure/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:52.314383-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串解析日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?
在Clojure中解析字符串中的日期是将日期和时间的文本表现形式转换成更加可用的形态（例如，Clojure的DateTime对象）。这个过程对于数据处理、日志记录或任何操作时间数据的应用程序来说是基础，使得程序员能够有效地执行日期的操作、比较或处理任务。

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
