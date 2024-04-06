---
date: 2024-01-20 17:32:36.001383-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u6BD4\u8F83\u65E5\u671F\u4F7F\
  \u7528\u4E86Joda-Time\u5E93\uFF0CClojure\u793E\u533A\u4E2D\u5E7F\u6CDB\u4F7F\u7528\
  \u7684\u65E5\u671F\u548C\u65F6\u95F4\u5E93\u662F`clj-time`\u3002Joda-Time\u7684\u5386\
  \u53F2\u53EF\u8FFD\u6EAF\u5230Java\u4E16\u754C\uFF0C\u662F\u5BF9Java\u65E9\u671F\
  \u65E5\u671F\u5904\u7406\u4E0D\u8DB3\u7684\u4E00\u79CD\u6539\u8FDB\u3002\u53EF\u66FF\
  \u4EE3\u7684\u65B9\u5F0F\u5305\u62ECJava\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.668483-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u6BD4\u8F83\u65E5\u671F\u4F7F\u7528\u4E86\
  Joda-Time\u5E93\uFF0CClojure\u793E\u533A\u4E2D\u5E7F\u6CDB\u4F7F\u7528\u7684\u65E5\
  \u671F\u548C\u65F6\u95F4\u5E93\u662F`clj-time`\u3002Joda-Time\u7684\u5386\u53F2\u53EF\
  \u8FFD\u6EAF\u5230Java\u4E16\u754C\uFF0C\u662F\u5BF9Java\u65E9\u671F\u65E5\u671F\
  \u5904\u7406\u4E0D\u8DB3\u7684\u4E00\u79CD\u6539\u8FDB\u3002\u53EF\u66FF\u4EE3\u7684\
  \u65B9\u5F0F\u5305\u62ECJava 8\u7684`java.time`\u5E93\u548CClojure\u7684\u5185\u7F6E\
  \u51FD\u6570\u3002\u5728\u5904\u7406\u65E5\u671F\u6BD4\u8F83\u65F6\uFF0C\u9700\u8981\
  \u8003\u8651\u65F6\u533A\u548C\u590F\u4EE4\u65F6\u7684\u5F71\u54CD\u3002"
title: "\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F"
weight: 27
---

## How to: (如何操作：)
```Clojure
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])

(let [date1 (time/date-time 2021 3 10)
      date2 (time/date-time 2023 2 20)]
  {:date1 date1
   :date2 date2
   :after? (time/after? date1 date2)
   :before? (time/before? date1 date2)
   :equal? (time/equal? date1 date2)})
   
;; 输出: {:date1 #object[org.joda.time.DateTime 0x4767c20a "2021-03-10T00:00:00.000Z"],
;;       :date2 #object[org.joda.time.DateTime 0x20453aee "2023-02-20T00:00:00.000Z"],
;;       :after? false,
;;       :before? true,
;;       :equal? false}
```

## Deep Dive (深入探讨)
比较日期使用了Joda-Time库，Clojure社区中广泛使用的日期和时间库是`clj-time`。Joda-Time的历史可追溯到Java世界，是对Java早期日期处理不足的一种改进。可替代的方式包括Java 8的`java.time`库和Clojure的内置函数。在处理日期比较时，需要考虑时区和夏令时的影响。

## See Also (参考链接)
- [clj-time GitHub repository](https://github.com/clj-time/clj-time)
- [Joda-Time](https://www.joda.org/joda-time/)
