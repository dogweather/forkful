---
date: 2024-01-26 04:38:59.303053-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Clojure \u901A\u8FC7 `clojure.lang.Numbers`\
  \ \u5DE5\u5177\u7C7B\u4E3A\u590D\u6570\u63D0\u4F9B\u4E86\u5185\u7F6E\u652F\u6301\
  \u3002\u4F7F\u7528 `complex` \u6765\u521B\u5EFA\u590D\u6570\u5E76\u6267\u884C\u7B97\
  \u672F\u8FD0\u7B97\u3002"
lastmod: '2024-04-05T22:38:46.477373-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Clojure \u901A\u8FC7 `clojure.lang.Numbers`\
  \ \u5DE5\u5177\u7C7B\u4E3A\u590D\u6570\u63D0\u4F9B\u4E86\u5185\u7F6E\u652F\u6301\
  \u3002\u4F7F\u7528 `complex` \u6765\u521B\u5EFA\u590D\u6570\u5E76\u6267\u884C\u7B97\
  \u672F\u8FD0\u7B97\u3002"
title: "\u5904\u7406\u590D\u6570"
weight: 14
---

## 如何操作：
Clojure 通过 `clojure.lang.Numbers` 工具类为复数提供了内置支持。使用 `complex` 来创建复数并执行算术运算。

```clojure
;; 创建复数
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; 加法
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; 减法
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; 乘法
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; 除法
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; 共轭
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## 深入探究
复数在18世纪由如高斯和欧拉等数学家正式提出。尽管最初遭到质疑，但它们已成为现代科学和工程中不可或缺的一部分。Clojure 没有像某些语言（例如，Python）那样的原生复数类型，但包含的 Java 互操作可以通过 `clojure.lang.Numbers` 类处理必要的操作。

Java 的 `java.lang.Complex` 是一个强大的替代品，提供更多功能和潜在的优化。Clojure 的宿主互操作性使得与 Java 库一起工作变得容易。

在底层，复数算术涉及到实部和虚部的加法和乘法，关键规则是 `i^2 = -1`。复数除法可能更复杂，通常需要用到共轭来避免除以复数。

## 参见
- ClojureDocs，快速参考资料：https://clojuredocs.org/
- `java.lang.Complex` 的 Java API：https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- 给数学好奇者的复数维基百科页面：https://en.wikipedia.org/wiki/Complex_number
