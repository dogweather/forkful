---
title:                "写入标准错误"
aliases:
- /zh/clojure/writing-to-standard-error/
date:                  2024-02-03T19:32:44.638636-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
将错误消息和诊断信息写入标准错误（stderr）是指将这些信息定向到stderr流，与标准输出（stdout）分开。程序员这样做是为了区分常规程序输出和错误消息，从而实现更有效的调试和记录。

## 如何操作：
在Clojure中，你可以使用`*err*`流写入stderr。这里是一个基本示例：

```clojure
(.write *err* "这是一条错误信息。\n")
```

注意，在写入消息后，你应该刷新流，以确保消息立即输出：

```clojure
(flush)
```

stderr的示例输出：
```
这是一条错误信息。
```

如果你在处理异常，可能会想要将栈跟踪打印到stderr。使用`printStackTrace`来做这件事：

```clojure
(try
  ;; 可能抛出异常的代码
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

对于更结构化的错误记录，第三方库如`timbre`可以配置为记录到stderr。这是一个基本的设置和使用方法：

首先，将`timbre`添加到你的依赖中。然后配置它使用stderr：

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; 禁用stdout日志
(timbre/set-config! [:appenders :spit :enabled?] false) ;; 禁用文件日志
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; 对于错误启用stderr

(timbre/error "处理您的请求时发生错误。")
```

这将会将错误级别的消息定向到stderr，使它们与标准应用程序输出区别开来。
