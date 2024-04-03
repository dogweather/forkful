---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:44.638636-07:00
description: "\u5C06\u9519\u8BEF\u6D88\u606F\u548C\u8BCA\u65AD\u4FE1\u606F\u5199\u5165\
  \u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\u662F\u6307\u5C06\u8FD9\u4E9B\u4FE1\u606F\
  \u5B9A\u5411\u5230stderr\u6D41\uFF0C\u4E0E\u6807\u51C6\u8F93\u51FA\uFF08stdout\uFF09\
  \u5206\u5F00\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u533A\u5206\
  \u5E38\u89C4\u7A0B\u5E8F\u8F93\u51FA\u548C\u9519\u8BEF\u6D88\u606F\uFF0C\u4ECE\u800C\
  \u5B9E\u73B0\u66F4\u6709\u6548\u7684\u8C03\u8BD5\u548C\u8BB0\u5F55\u3002"
lastmod: '2024-03-13T22:44:47.321541-06:00'
model: gpt-4-0125-preview
summary: "\u5C06\u9519\u8BEF\u6D88\u606F\u548C\u8BCA\u65AD\u4FE1\u606F\u5199\u5165\
  \u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\u662F\u6307\u5C06\u8FD9\u4E9B\u4FE1\u606F\
  \u5B9A\u5411\u5230stderr\u6D41\uFF0C\u4E0E\u6807\u51C6\u8F93\u51FA\uFF08stdout\uFF09\
  \u5206\u5F00\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u533A\u5206\
  \u5E38\u89C4\u7A0B\u5E8F\u8F93\u51FA\u548C\u9519\u8BEF\u6D88\u606F\uFF0C\u4ECE\u800C\
  \u5B9E\u73B0\u66F4\u6709\u6548\u7684\u8C03\u8BD5\u548C\u8BB0\u5F55\u3002."
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

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
