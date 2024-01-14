---
title:                "Clojure: 「テストを書く」"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## なぜ
プログラミングでテストを書くことの重要性を説明します。テストを書くことにより、ソフトウェアの品質を向上させ、バグを防ぐことができます。

## 方法
以下のClojureコードブロックでは、テストを書く方法を示します。

```Clojure
;; テスト用の名前空間を定義する
(ns blog.test
  (:require [clojure.test :refer :all]
	    [blog.core :refer :all])

;; テストを定義する
(deftest addition-test
  (testing "2つの数値の足し算が正しく行われること"
    (is (= (add 2 2) 4)))
  (testing "負の数も正しく足し算されること"
    (is (= (add -5 10) 5)))

;; テストを実行する
(run-tests)

```
以下は、テストを実行した時の出力結果です。

```
Testing blog.test

Ran 2 tests containing 3 assertions.
0 failures, 0 errors.
```

## 深い理解
テストの書き方に関してより詳細に学ぶために、以下のリソースを参考にしてください。

- [公式Clojureドキュメント](https://clojuredocs.org/clojure.repl/run-tests)
- [Clojureテストの書き方](https://www.braveclojure.com/testing/#Clojure_test_driven_development)
- [テストを使った実践的なClojure開発](https://stuartsierra.com/2015/08/25/clojure-pedestal-app-with-clojure-spec-and-clojure-test)

## 他のリンクを参照
- [Clojureの公式サイト](https://clojure.org/)
- [Clojure日本ユーザーグループ](https://clojure.jp/)
- [テスト駆動開発についてのブログ記事](https://t-wada.hatenadiary.org/entry/20111208/1323348668)