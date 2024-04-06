---
date: 2024-01-26 03:43:53.263370-07:00
description: "\u65B9\u6CD5\uFF1A Clojure\u3067\u306F\u3001\u4E3B\u306B`Math/round`\u3001\
  `Math/floor`\u3001\u305D\u3057\u3066`Math/ceil`\u3092\u4F7F\u7528\u3057\u307E\u3059\
  \uFF1A."
lastmod: '2024-04-05T22:37:49.881562-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Clojure\u3067\u306F\u3001\u4E3B\u306B`Math/round`\u3001\
  `Math/floor`\u3001\u305D\u3057\u3066`Math/ceil`\u3092\u4F7F\u7528\u3057\u307E\u3059\
  \uFF1A."
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
weight: 13
---

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
