---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 字串長さの把握、その手法と必要性

## 何か & なぜそれか？

文字列の長さとは文字列に含まれる文字の総数を指します。文節、単語、またはフレーズでなければならない場合や、文字列の長さが一定の範囲に収まっている必要がある場合など、プログラマがこれを把握する理由はさまざまです。

## 実装方法

Clojureにおける文字列の長さの取得は、`count`関数を使います。例えば：

```Clojure
(defn string-length-example []
  (let [str "Clojureで学べることは無数にある！"]
    (println "定義した文字列は: " str)
    (println "文字列の長さは: " (count str))))
```
これを走らせると、次のような出力が得られます：

```Clojure
定義した文字列は: Clojureで学べることは無数にある！
文字列の長さは: 19
```
つまりcount関数は、文字列の長さを数値で返すことが分かります。

## 詳細

文字列の長さを計算するという概念は、プログラミングの初期から存在しています。ほとんどのプログラミング言語にはこの機能があり、それぞれが独自の実装を持っています。Clojureでは、文字列の長さを計算する`count`関数が提供されています。

考え得る代替手段としては、文字列を順にスキャンし、文字ごとにカウンタをインクリメントする、という手法があります。このアプローチは特に低レベル言語でよく見られます。しかし、Clojureのような高レベル言語では、`count`関数のような組み込み機能を利用することが一般的です。

`count`関数の内部的な動作については、ClojureがJVM上で動作する言語であることを考えると、実際の実装はJavaの`String.length()`メソッドを利用していると推測することができます。詳細はClojureのソースコードを参照してください：

## 参考リンク

1. Clojureで使用可能な他の組み込み関数については、公式ドキュメントを参照してください：
   [Clojure公式ドキュメント](https://clojure.org/api/api)

2. Javaの`length()`メソッドについては、Java公式ドキュメントを参照してください：
   [Java Stringクラス公式ドキュメント](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)