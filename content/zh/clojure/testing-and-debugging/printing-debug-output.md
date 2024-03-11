---
date: 2024-01-20 17:52:29.845754-07:00
description: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u5C31\u662F\u5728\u4EE3\u7801\u4E2D\
  \u663E\u793A\u53D8\u91CF\u7684\u503C\u6216\u7A0B\u5E8F\u8FD0\u884C\u72B6\u6001\uFF0C\
  \u4FBF\u4E8E\u89C2\u5BDF\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\
  \u5FEB\u901F\u53D1\u73B0\u9519\u8BEF\u548C\u7406\u89E3\u7A0B\u5E8F\u884C\u4E3A\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.074698-06:00'
model: gpt-4-1106-preview
summary: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u5C31\u662F\u5728\u4EE3\u7801\u4E2D\
  \u663E\u793A\u53D8\u91CF\u7684\u503C\u6216\u7A0B\u5E8F\u8FD0\u884C\u72B6\u6001\uFF0C\
  \u4FBF\u4E8E\u89C2\u5BDF\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\
  \u5FEB\u901F\u53D1\u73B0\u9519\u8BEF\u548C\u7406\u89E3\u7A0B\u5E8F\u884C\u4E3A\u3002"
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
打印调试输出就是在代码中显示变量的值或程序运行状态，便于观察。程序员这样做是为了快速发现错误和理解程序行为。

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
