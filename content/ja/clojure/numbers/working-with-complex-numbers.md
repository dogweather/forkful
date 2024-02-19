---
aliases:
- /ja/clojure/working-with-complex-numbers/
date: 2024-01-26 04:39:12.264154-07:00
description: "\u8907\u7D20\u6570\u306F\u3001\u5B9F\u6570\u306B\u865A\u6570\u5358\u4F4D\
  \ 'i' \u3092\u52A0\u3048\u305F\u3082\u306E\u3067\u3001\u8CA0\u306E\u6570\u306E\u5E73\
  \u65B9\u6839\u3092\u542B\u3080\u8A08\u7B97\u304C\u65E5\u5E38\u7684\u306B\u884C\u308F\
  \u308C\u308B\u4FE1\u53F7\u51E6\u7406\u3001\u96FB\u78C1\u6C17\u7406\u8AD6\u3001\u30D5\
  \u30E9\u30AF\u30BF\u30EB\u306A\u3069\u3001\u69D8\u3005\u306A\u9818\u57DF\u3067\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306B\u3088\u3063\u3066\u4F7F\u7528\u3055\u308C\u307E\
  \u3059\u3002"
lastmod: 2024-02-18 23:08:54.598054
model: gpt-4-0125-preview
summary: "\u8907\u7D20\u6570\u306F\u3001\u5B9F\u6570\u306B\u865A\u6570\u5358\u4F4D\
  \ 'i' \u3092\u52A0\u3048\u305F\u3082\u306E\u3067\u3001\u8CA0\u306E\u6570\u306E\u5E73\
  \u65B9\u6839\u3092\u542B\u3080\u8A08\u7B97\u304C\u65E5\u5E38\u7684\u306B\u884C\u308F\
  \u308C\u308B\u4FE1\u53F7\u51E6\u7406\u3001\u96FB\u78C1\u6C17\u7406\u8AD6\u3001\u30D5\
  \u30E9\u30AF\u30BF\u30EB\u306A\u3069\u3001\u69D8\u3005\u306A\u9818\u57DF\u3067\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306B\u3088\u3063\u3066\u4F7F\u7528\u3055\u308C\u307E\
  \u3059\u3002"
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？
複素数は、実数に虚数単位 'i' を加えたもので、負の数の平方根を含む計算が日常的に行われる信号処理、電磁気理論、フラクタルなど、様々な領域でプログラマーによって使用されます。

## 使い方
Clojureは、`clojure.lang.Numbers` ユーティリティクラスを通じて複素数の組み込みサポートを提供しています。`complex`を使用して複素数を作成し、算術演算を行います。

```clojure
;; 複素数の作成
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; 加算
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; 減算
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; 乗算
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; 除算
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; 共役
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## 深堀り
複素数は、18世紀にガウスやオイラーのような数学者によって公式化されました。当初は懐疑的に受け止められましたが、現代の科学と工学において不可欠なものとなっています。Clojureはいくつかの言語（例えば、Python）のようにネイティブの複素数型を持っていませんが、`clojure.lang.Numbers` クラスを介したJavaのインターオペレーションが必要な演算を処理できます。

Javaの `java.lang.Complex` は、より多くの機能と潜在的な最適化を提供する堅牢な代替手段です。Clojureのホスト相互運用性により、Javaライブラリを使いやすくします。

内部的には、複素数の算術演算は、`i^2 = -1`という主要なルールを用いて、実部と虚部の加算と乗算を含みます。複素数による除算はより複雑になりがちで、通常は複素数による除算を避けるため共役を使用する必要があります。

## 参照
- クイックリファレンスとしてのClojureDocs: https://clojuredocs.org/
- `java.lang.Complex`のJava API: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- 数学的に好奇心がある人のための複素数に関するWikipediaページ: https://en.wikipedia.org/wiki/Complex_number
