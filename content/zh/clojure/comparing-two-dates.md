---
title:                "比较两个日期"
date:                  2024-01-20T17:32:36.001383-07:00
model:                 gpt-4-1106-preview
simple_title:         "比较两个日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/comparing-two-dates.md"
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
