---
title:                "获取当前日期"
date:                  2024-02-03T19:09:13.360302-07:00
model:                 gpt-4-0125-preview
simple_title:         "获取当前日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
在编程中获取当前日期是至关重要的，原因包括记录日志、时间戳事件和安排任务。在 Clojure 中，一个基于 JVM 的 Lisp 方言，这个任务利用了 Java 互操作功能，允许直接访问丰富的 Java 日期时间 API。

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
