---
title:                "ランダム数の生成"
date:                  2024-01-20T17:48:46.017969-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ランダムな数値生成とは、予測できない数値を生み出すプロセスです。プログラマーは、テストデータの作成、ゲームの要素のランダマイズ、セキュリティ（例えば、暗号化）などのためにこれを行います。

## How to (やり方)
Clojureでランダムな数値を生成する基本的な方法を紹介します。

```clojure
;; 整数のランダムな数を生成
(rand-int 10)

;; 0から1までの浮動小数点数のランダムな数を生成
(rand)

;; シード値を指定してランダムジェネレータを作成
(def r (java.util.Random. 1234))

;; シード値を持つジェネレータから整数を生成
(.nextInt r 10)
```

実行結果は毎回異なりますが、シード値を設定した場合は同じシードから同じ数値列が生成されます。

## Deep Dive (深く掘り下げる)
ランダム数値生成は、統計、物理学、暗号学などに根ざした長い歴史を持ちます。最初は物理的な手法（例：さいころの投げ）でしたが、アルゴリズムによる疑似ランダム数値生成が開発されました。

ClojureはJavaプラットフォーム上に構築されているため、java.util.RandomクラスなどのJavaメソッドも使用可能です。疑似ランダムではなく実際のランダム性が必要な場合、java.security.SecureRandomがより安全です。

生成のアルゴリズムは、平均的に分布する「一様分布」から、特定の分布（正規分布など）を模倣するものまで多岐にわります。Clojure（およびJava）では、これらの異なる分布を扱うためのツールが提供されています。

## See Also (参考情報)
- [Clojure Documentation](https://clojure.org/guides/learn/functions#_random_numbers)
- [Clojureでのランダム数値生成](https://clojuredocs.org/clojure.core/rand)
- [JavaのRandomクラス](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
