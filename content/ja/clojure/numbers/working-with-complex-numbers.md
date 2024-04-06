---
date: 2024-01-26 04:39:12.264154-07:00
description: "\u4F7F\u3044\u65B9 Clojure\u306F\u3001`clojure.lang.Numbers` \u30E6\u30FC\
  \u30C6\u30A3\u30EA\u30C6\u30A3\u30AF\u30E9\u30B9\u3092\u901A\u3058\u3066\u8907\u7D20\
  \u6570\u306E\u7D44\u307F\u8FBC\u307F\u30B5\u30DD\u30FC\u30C8\u3092\u63D0\u4F9B\u3057\
  \u3066\u3044\u307E\u3059\u3002`complex`\u3092\u4F7F\u7528\u3057\u3066\u8907\u7D20\
  \u6570\u3092\u4F5C\u6210\u3057\u3001\u7B97\u8853\u6F14\u7B97\u3092\u884C\u3044\u307E\
  \u3059\u3002"
lastmod: '2024-04-05T22:37:49.880632-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u3044\u65B9 Clojure\u306F\u3001`clojure.lang.Numbers` \u30E6\u30FC\
  \u30C6\u30A3\u30EA\u30C6\u30A3\u30AF\u30E9\u30B9\u3092\u901A\u3058\u3066\u8907\u7D20\
  \u6570\u306E\u7D44\u307F\u8FBC\u307F\u30B5\u30DD\u30FC\u30C8\u3092\u63D0\u4F9B\u3057\
  \u3066\u3044\u307E\u3059\u3002`complex`\u3092\u4F7F\u7528\u3057\u3066\u8907\u7D20\
  \u6570\u3092\u4F5C\u6210\u3057\u3001\u7B97\u8853\u6F14\u7B97\u3092\u884C\u3044\u307E\
  \u3059\u3002"
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

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
