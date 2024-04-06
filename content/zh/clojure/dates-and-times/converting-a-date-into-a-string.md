---
date: 2024-01-20 17:36:03.315927-07:00
description: "How to: \u6DF1\u5165\u7814\u7A76: \u65E9\u671F\u5728Java\u5E73\u53F0\
  \u4E0A\u5904\u7406\u65F6\u95F4\u548C\u65E5\u671F\u4F7F\u7528\u7684\u662F`java.util.Date`\uFF0C\
  \u4F46\u662F\u5176\u8BBE\u8BA1\u6709\u9650\u5236\uFF0C\u6BD4\u5982\u7EBF\u7A0B\u5B89\
  \u5168\u95EE\u9898\u548C\u53EF\u8BFB\u6027\u5DEE\u3002`clj-time`\u5E93\u57FA\u4E8E\
  Joda-Time\uFF0C\u7ED9Clojure\u63D0\u4F9B\u4E86\u66F4\u597D\u7684\u65E5\u671F\u65F6\
  \u95F4API\u3002\u9664\u4E86`clj-\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:46.498006-06:00'
model: gpt-4-1106-preview
summary: "\u6DF1\u5165\u7814\u7A76: \u65E9\u671F\u5728Java\u5E73\u53F0\u4E0A\u5904\
  \u7406\u65F6\u95F4\u548C\u65E5\u671F\u4F7F\u7528\u7684\u662F`java.util.Date`\uFF0C\
  \u4F46\u662F\u5176\u8BBE\u8BA1\u6709\u9650\u5236\uFF0C\u6BD4\u5982\u7EBF\u7A0B\u5B89\
  \u5168\u95EE\u9898\u548C\u53EF\u8BFB\u6027\u5DEE\u3002`clj-time`\u5E93\u57FA\u4E8E\
  Joda-Time\uFF0C\u7ED9Clojure\u63D0\u4F9B\u4E86\u66F4\u597D\u7684\u65E5\u671F\u65F6\
  \u95F4API\u3002\u9664\u4E86`clj-time`\uFF0CClojure\u5F00\u53D1\u8005\u73B0\u5728\
  \u4E5F\u53EF\u4EE5\u4F7F\u7528`java.time`\u5E93\uFF0C\u5B83\u5728Java 8\u5F15\u5165\
  \uFF0C\u8BBE\u8BA1\u66F4\u52A0\u73B0\u4EE3\u3002\u65E5\u671F\u8F6C\u6362\u6210\u5B57\
  \u7B26\u4E32\u65F6\uFF0C\u683C\u5F0F\u5FC5\u987B\u660E\u786E\uFF0C\u53EF\u4EE5\u7528\
  \u6807\u51C6\u683C\u5F0F\u4E5F\u53EF\u4EE5\u81EA\u5B9A\u4E49\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

## How to:
如何操作:
```Clojure
;; 引入 Clojure 日期时间库
(require '[clj-time.core :as t])
(require '[clj-time.format :as f])

;; 创建一个日期时间对象
(def my-date (t/now))

;; 定义日期时间格式
(def formatter (f/formatters :basic-date-time))

;; 把日期转换成字符串
(def date-str (f/unparse formatter my-date))

;; 打印结果
(println date-str)
```
```
;; 示例输出
"20230404T101015Z"
```

## Deep Dive
深入研究: 早期在Java平台上处理时间和日期使用的是`java.util.Date`，但是其设计有限制，比如线程安全问题和可读性差。`clj-time`库基于Joda-Time，给Clojure提供了更好的日期时间API。除了`clj-time`，Clojure开发者现在也可以使用`java.time`库，它在Java 8引入，设计更加现代。日期转换成字符串时，格式必须明确，可以用标准格式也可以自定义。

## See Also
参考链接:
- clj-time GitHub: https://github.com/clj-time/clj-time
- Joda-Time: https://www.joda.org/joda-time/
- Clojure official documentation: https://clojure.org/guides/deps_and_cli
- Java Time (java.time): https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
