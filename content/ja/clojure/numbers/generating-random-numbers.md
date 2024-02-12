---
title:                "乱数の生成"
date:                  2024-01-27T20:33:08.057131-07:00
model:                 gpt-4-0125-preview
simple_title:         "乱数の生成"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 何となぜ？

プログラミングにおける乱数の生成とは、事前に論理的に予測できない値を作り出すことです。プログラマーは、一意な識別子を生成する、ゲーム開発でシナリオをシミュレートする、データからランダムなサンプルを選択する分析を行うなど、さまざまな理由でこれを行います。

## 方法：

Clojureでは、乱数生成は非常に簡単で、すぐに使えるいくつかの組み込み関数があります。

0（含む）と1（含まない）の間のランダムな浮動小数点数を生成するには、`rand`関数を使用できます：

```Clojure
(rand)
;; 例の出力: 0.7094245047062917
```

特定の範囲内の整数が必要な場合は、`rand-int`を使います：

```Clojure
(rand-int 10)
;; 例の出力: 7
```

これにより、0（含む）と引数として渡した数（含まない）の間のランダムな整数が得られます。

特定の範囲内（整数に限らない）のランダムな数値を生成するには、算術演算と`rand`を組み合わせることができます：

```Clojure
(defn rand-range [min max]
  (+ min (* (rand) (- max min))))
;; 使用法
(rand-range 10 20)
;; 例の出力: 14.857457734992847
```

この関数`rand-range`は、指定した`min`と`max`の値の間のランダムな浮動小数点数を返します。

繰り返し可能な乱数の複雑な分布やシーケンスが必要なシナリオ（シードを使用）では、組み込みの機能を超えた追加のライブラリを調べる必要があります。

## 深掘り

ほとんどのプログラミング言語、Clojureを含む乱数生成の基本メカニズムは、通常、擬似乱数生成器（PRNG）に依存しています。PRNGは、アルゴリズムを使用して、乱数の特性を近似する一連の数値を生成します。これらはアルゴリズムで生成されたものであるため、真にランダムではありませんが、ほとんどの実用的な目的には十分です。

コンピューティングの初期の頃、高品質な乱数を生成することは重大な課題であり、ランダム性と分布を改善するさまざまなアルゴリズムの開発につながりました。`rand`や`rand-int`などのClojureの組み込み関数は、日常使用に便利で、一般的な用途の広い範囲をカバーします。

しかし、暗号セキュリティが必要なアプリケーションや、より複雑な統計的サンプリング方法が必要な場合、Clojure開発者は、より堅牢で特化したPRNGを提供する外部ライブラリに頻繁に頼ります。`clj-random`などのライブラリは、より幅広いアルゴリズムへのアクセスと、シミュレーション、暗号アプリケーション、またはランダム数列の品質と予測可能性が重要な意味を持つ可能性のあるどのドメインでも、シードに対するより大きな制御を提供します。

Clojureの乱数生成の組み込み機能は多くのタスクに適していますが、外部ライブラリの探索は、特定またはより重要なアプリケーションのためのより深い洞察と選択肢を提供することができます。