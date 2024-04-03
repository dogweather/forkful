---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:13.360302-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A #."
lastmod: '2024-03-13T22:44:47.314850-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

## 如何操作：


### 使用 Java 互操作
Clojure 与 Java 的无缝互操作性允许你直接利用 Java 日期时间 API。以下是获取当前日期的方法：

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; 示例输出
(get-current-date) ; "2023-04-15"
```

### 使用 clj-time 库
对于一个更符合 Clojure 习惯的解决方案，你可能会选择 `clj-time` 库，一个围绕 Joda-Time 的封装，虽然对于大多数新项目，建议使用内置的 Java 8 日期时间 API。然而，如果你偏好或需要 `clj-time`：

首先，将 `clj-time` 添加到你的项目依赖中。在你的 `project.clj` 中，包含：

```clojure
[clj-time "0.15.2"]
```

然后，使用它来获取当前日期：

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; 示例输出
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

这两种方法都提供了在 Clojure 中快速有效地获取当前日期的方式，利用了底层 Java 平台的力量或 Clojure 特定库的便利。
