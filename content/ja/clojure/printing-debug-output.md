---
title:                "Clojure: デバッグの出力を出力する"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

デバッグ出力をプリントするのに、なぜ人々が従事するのかを説明するために1-2文を記述します。

デバッグ出力をプリントすることは、プログラムの実行時に特定の値や変数の値を確認することができるため、コードの動作を理解する上で非常に有用です。また、ユーザーへのエラー情報の提供や、コードのパフォーマンスの改善にも役立ちます。

## 方法

プログラミング言語Clojureでは、デバッグ出力のために `println` という関数を使用します。以下のようにコードブロック内にClojureのコードを書くことで、実際の動作を確認することができます。

```Clojure
(defn add-two [n]
  (println "Calculating sum of" n "and 2...")
  (+ n 2))

(add-two 5)

;; 出力:
;; Calculating sum of 5 and 2...
;; 7
```

このように、 `println` 関数を使用することで、任意の値や変数の値を出力することができます。また、文字列と変数・値を組み合わせて出力することもできます。これにより、コードの動作や実行結果を可視化することができます。

## ディープダイブ

デバッグ出力をプリントする方法は、プログラムの実行中に随時行うことができます。また、特定の条件下でのみ出力するように条件分岐を組み合わせることも可能です。

また、Clojureではデバッグ用のマクロとして `debug` が用意されています。これを使用することで、デバッグ出力を簡単に行うことができます。詳細については、公式ドキュメントを参照してください。

## 参考リンク

- Clojureの`println`関数についての公式ドキュメント: https://clojuredocs.org/clojure.core/println
- 条件分岐を使用したデバッグ出力の方法についてのブログ記事: https://blog.clojure.org/2017/09/14/debugging
- Clojureの `debug` マクロについての公式ドキュメント: https://clojuredocs.org/clojure.core/debug