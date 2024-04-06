---
date: 2024-01-20 17:52:29.845754-07:00
description: "How to: \u5982\u4F55\u64CD\u4F5C\uFF1F \u6253\u5370\u8C03\u8BD5\u4FE1\
  \u606F\u662F\u7A0B\u5E8F\u5458\u81EA\u53E4\u4EE5\u6765\u5C31\u4F7F\u7528\u7684\u6280\
  \u672F\u3002\u5728Clojure\u4E2D\uFF0C`println` \u662F\u6700\u76F4\u63A5\u7684\u65B9\
  \u5F0F\uFF0C\u4F46\u8C03\u8BD5\u66F4\u590D\u6742\u7684\u7CFB\u7EDF\u65F6\u53EF\u80FD\
  \u9700\u8981\u65E5\u5FD7\u5E93\uFF0C\u6BD4\u5982 `timbre`\u3002 Clojure\u7684\u51FD\
  \u6570\u5F0F\u7279\u6027\u8BA9\u6211\u4EEC\u53EF\u4EE5\u5DE7\u5999\u5730\u63D2\u5165\
  \u6253\u5370\u8BED\u53E5\u800C\u4E0D\u5E72\u6270\u539F\u6709\u4EE3\u7801\u903B\u8F91\
  \u3002\u4F8B\u5982\uFF0C\u53EF\u4EE5\u4F7F\u7528 `tap>`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.657269-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1F \u6253\u5370\u8C03\u8BD5\u4FE1\u606F\u662F\
  \u7A0B\u5E8F\u5458\u81EA\u53E4\u4EE5\u6765\u5C31\u4F7F\u7528\u7684\u6280\u672F\u3002\
  \u5728Clojure\u4E2D\uFF0C`println` \u662F\u6700\u76F4\u63A5\u7684\u65B9\u5F0F\uFF0C\
  \u4F46\u8C03\u8BD5\u66F4\u590D\u6742\u7684\u7CFB\u7EDF\u65F6\u53EF\u80FD\u9700\u8981\
  \u65E5\u5FD7\u5E93\uFF0C\u6BD4\u5982 `timbre`."
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

## How to: 如何操作？
```Clojure
;; 打印一个简单的字符串到控制台
(println "Debugging output starts here: ")

;; 打印变量的值
(def my-var "Hello, Debugger!")
(println my-var)

;; 使用格式化字符串打印
(defn debug-info
  [var]
  (println (str "Debug for var: " var)))

(debug-info my-var)

;; Sample Output 样例输出
; Debugging output starts here: 
; Hello, Debugger!
; Debug for var: Hello, Debugger!
```

## Deep Dive 深入探讨
打印调试信息是程序员自古以来就使用的技术。在Clojure中，`println` 是最直接的方式，但调试更复杂的系统时可能需要日志库，比如 `timbre`。

Clojure的函数式特性让我们可以巧妙地插入打印语句而不干扰原有代码逻辑。例如，可以使用 `tap>` 在不改变数据流的情况下观察变量。

替代方法包括使用REPL进行交互式调试或者使用诸如 CIDER 的工具进行跟踪调试。而实现细节关键在于理解，`println` 实际上是写入到 `*out*` 这个标准输出流。

## See Also 参考资料
- [Clojure - println documentation](https://clojuredocs.org/clojure.core/println)
- [Clojure Debugging Tool - CIDER](https://docs.cider.mx/cider/)
- [Timbre - logging library for Clojure](https://github.com/ptaoussanis/timbre)
- [Using tap> in Clojure](https://clojure.org/guides/faq#tap)
