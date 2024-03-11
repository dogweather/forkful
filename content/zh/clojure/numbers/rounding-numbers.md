---
date: 2024-01-26 03:43:33.888055-07:00
description: "\u820D\u5165\u6570\u5B57\u662F\u6307\u5C06\u4E00\u4E2A\u6570\u5B57\u8C03\
  \u6574\u5230\u6700\u63A5\u8FD1\u7684\u6574\u6570\uFF0C\u6216\u8005\u8C03\u6574\u5230\
  \u67D0\u4E2A\u7279\u5B9A\u7684\u5C0F\u6570\u7CBE\u5EA6\u3002\u6211\u4EEC\u5BF9\u6570\
  \u5B57\u8FDB\u884C\u820D\u5165\u662F\u4E3A\u4E86\u7B80\u5316\u5B83\u4EEC\uFF0C\u4FBF\
  \u4E8E\u4EBA\u7C7B\u9605\u8BFB\uFF0C\u51CF\u5C11\u8BA1\u7B97\u8D1F\u8377\uFF0C\u6216\
  \u6EE1\u8DB3\u7279\u5B9A\u7684\u6570\u5B57\u8981\u6C42\u3002"
lastmod: '2024-03-11T00:14:21.066374-06:00'
model: gpt-4-0125-preview
summary: "\u820D\u5165\u6570\u5B57\u662F\u6307\u5C06\u4E00\u4E2A\u6570\u5B57\u8C03\
  \u6574\u5230\u6700\u63A5\u8FD1\u7684\u6574\u6570\uFF0C\u6216\u8005\u8C03\u6574\u5230\
  \u67D0\u4E2A\u7279\u5B9A\u7684\u5C0F\u6570\u7CBE\u5EA6\u3002\u6211\u4EEC\u5BF9\u6570\
  \u5B57\u8FDB\u884C\u820D\u5165\u662F\u4E3A\u4E86\u7B80\u5316\u5B83\u4EEC\uFF0C\u4FBF\
  \u4E8E\u4EBA\u7C7B\u9605\u8BFB\uFF0C\u51CF\u5C11\u8BA1\u7B97\u8D1F\u8377\uFF0C\u6216\
  \u6EE1\u8DB3\u7279\u5B9A\u7684\u6570\u5B57\u8981\u6C42\u3002"
title: "\u6570\u5B57\u53D6\u6574"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
舍入数字是指将一个数字调整到最接近的整数，或者调整到某个特定的小数精度。我们对数字进行舍入是为了简化它们，便于人类阅读，减少计算负荷，或满足特定的数字要求。

## 如何实现：
在 Clojure 中，我们主要使用 `Math/round`、`Math/floor` 和 `Math/ceil`：

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

针对特定的小数位，我们进行乘法、舍入和除法操作：

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## 深入探讨
在拥有先进编程语言之前，舍入是一个手动过程，比如算盘或纸张。在编程中，由于浮点数精度的限制，数字表示非常关键。

舍入的替代方法包括使用 `BigDecimal` 类来控制精度，或者使用像 `clojure.math.numeric-tower` 这样的库来进行高级数学功能计算。Clojure 的 `Math/round` 依赖于 Java 的 `Math.round`、`Math/floor` 和 `Math/ceil` 函数，这意味着它继承了同样的浮点数和双精度浮点数的细微差别。

在实现时，记住当 Clojure 处理小数时自动使用双精度浮点数。小心舍入误差！

## 另请参阅
- Clojure 数学 API: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- Java 数学 API: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- 理解浮点数精度：[https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
