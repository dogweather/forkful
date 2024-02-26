---
date: 2024-01-26 03:43:53.263370-07:00
description: "\u6570\u5024\u3092\u4E38\u3081\u308B\u3068\u3044\u3046\u306E\u306F\u3001\
  \u3042\u308B\u6570\u5B57\u3092\u6700\u3082\u8FD1\u3044\u5168\u4F53\u306E\u6570\u5024\
  \u3001\u307E\u305F\u306F\u7279\u5B9A\u306E\u5C0F\u6570\u70B9\u7CBE\u5EA6\u306B\u5FAE\
  \u8ABF\u6574\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u79C1\u305F\u3061\u306F\u3001\
  \u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u304F\u3059\u308B\u305F\u3081\u3001\u8A08\
  \u7B97\u8CA0\u8377\u3092\u6E1B\u3089\u3059\u305F\u3081\u3001\u307E\u305F\u306F\u7279\
  \u5B9A\u306E\u6570\u5024\u8981\u4EF6\u3092\u6E80\u305F\u3059\u305F\u3081\u306B\u6570\
  \u5024\u3092\u4E38\u3081\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:39.702860-07:00'
model: gpt-4-0125-preview
summary: "\u6570\u5024\u3092\u4E38\u3081\u308B\u3068\u3044\u3046\u306E\u306F\u3001\
  \u3042\u308B\u6570\u5B57\u3092\u6700\u3082\u8FD1\u3044\u5168\u4F53\u306E\u6570\u5024\
  \u3001\u307E\u305F\u306F\u7279\u5B9A\u306E\u5C0F\u6570\u70B9\u7CBE\u5EA6\u306B\u5FAE\
  \u8ABF\u6574\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u79C1\u305F\u3061\u306F\u3001\
  \u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u304F\u3059\u308B\u305F\u3081\u3001\u8A08\
  \u7B97\u8CA0\u8377\u3092\u6E1B\u3089\u3059\u305F\u3081\u3001\u307E\u305F\u306F\u7279\
  \u5B9A\u306E\u6570\u5024\u8981\u4EF6\u3092\u6E80\u305F\u3059\u305F\u3081\u306B\u6570\
  \u5024\u3092\u4E38\u3081\u307E\u3059\u3002"
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
---

{{< edit_this_page >}}

## 何となぜ？
数値を丸めるというのは、ある数字を最も近い全体の数値、または特定の小数点精度に微調整することです。私たちは、人間が読みやすくするため、計算負荷を減らすため、または特定の数値要件を満たすために数値を丸めます。

## 方法：
Clojureでは、主に`Math/round`、`Math/floor`、そして`Math/ceil`を使用します：

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

特定の小数点まで丸める場合、乗算、丸め、除算を行います：

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## 深く掘り下げて
かつて、高度なプログラミング言語がなかった時代には、数値の丸めは手作業で行われ、そう考えると珠算や紙を使ったものです。プログラミングでは、浮動小数点の精度の限界のため、数値表現にとって不可欠です。

丸めの代替手段には、精度管理のための`BigDecimal`クラスの使用や、`clojure.math.numeric-tower`のような高度な数学関数のライブラリがあります。Clojureの`Math/round`はJavaの`Math.round`、`Math/floor`、そして`Math/ceil`関数に依存しているため、同じ浮動小数点と倍精度のニュアンスを継承しています。

実装に関しては、Clojureで丸めを行う際には、小数を扱うと自動的に倍精度が使われることを覚えておいてください。丸め誤差には注意が必要です！

## 参照
- Clojure Math API: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- Java Math API: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- 浮動小数点精度を理解する: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
