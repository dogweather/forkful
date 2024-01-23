---
title:                "テストの作成"
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (テストの書き方とその理由)
テストとは、プログラムが正しく動く保証をするための方法です。バグを見つけ、将来の機能追加やリファクタリングに安心感を持たせるために、プログラマーはテストを書きます。

## How to: (やり方)
```Clojure
;; Clojureのテストライブラリを使う
(require '[clojure.test :refer :all])

;; サンプル関数定義
(defn add [a b]
  (+ a b))

;; テスト定義
(deftest test-add
  (testing "add関数のテスト"
    (is (= 3 (add 1 2)))
    (is (= 7 (add 3 4)))))

;; テスト実行
(run-tests)

;; サンプル出力
;; 
;; Testing user
;; 
;; 2 tests, 0 failures.
```

## Deep Dive (掘り下げ)
過去、テストはオプションだったが今では必須と考えられています。Clojureでは、`clojure.test`が標準ライブラリとしてテストをサポート。JVMのエコシステムにおいて、JUnitなど他のテストライブラリも選択肢として使えますが、Clojureの関数型特有のアプローチにはclojure.testが適しています。テストの記述はアサーションを使って行い、関数の出力が期待値と一致しているかを検証します。

## See Also (関連情報)
- Clojureの公式ドキュメントのテストセクション: https://clojure.org/guides/deps_and_cli#_testing
- `clojure.test`のドキュメント: https://clojure.github.io/clojure/clojure.test-api.html
- 実践的なClojureテスト戦略に関する記事: https://www.oreilly.com/library/view/clojure-programming/9781449310387/ch12.html
- JUnit: https://junit.org/junit4/
