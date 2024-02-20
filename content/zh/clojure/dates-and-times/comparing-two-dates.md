---
date: 2024-01-20 17:32:36.001383-07:00
description: "\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F\u5C31\u662F\u786E\u5B9A\u5B83\u4EEC\
  \u4E4B\u95F4\u7684\u5148\u540E\u6216\u5DEE\u5F02\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u6392\u5E8F\u3001\u6821\u9A8C\u6216\u8BA1\u7B97\u65F6\u95F4\
  \u8DE8\u5EA6\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.400554
model: gpt-4-1106-preview
summary: "\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F\u5C31\u662F\u786E\u5B9A\u5B83\u4EEC\
  \u4E4B\u95F4\u7684\u5148\u540E\u6216\u5DEE\u5F02\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u6392\u5E8F\u3001\u6821\u9A8C\u6216\u8BA1\u7B97\u65F6\u95F4\
  \u8DE8\u5EA6\u3002"
title: "\u6BD4\u8F83\u4E24\u4E2A\u65E5\u671F"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
比较两个日期就是确定它们之间的先后或差异。程序员这样做是为了排序、校验或计算时间跨度。

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
