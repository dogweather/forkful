---
title:                "テストの書き方"
html_title:           "Clojure: テストの書き方"
simple_title:         "テストの書き方"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## 何&なぜ？
テストを書くとは何か、そしてなぜプログラマーがそれを行うのかを説明する、2〜3文。

テストを書くことは、コードが期待どおりに動作するかを確認することです。プログラマーは、自分のコードが正しく動作することを保証するために、テストを書きます。

## 作り方：
 ```Clojure
 (defn add [x y]
   (+ x y))

 (add 2 3) ;; => 5
```
上記の例では、```add```関数が正しく動作するかを確認するために、2つの数字を渡しています。もし結果が期待通りでなければ、テストは失敗します。

## 深いダイブ：
テストを書くことは、コンピュータサイエンスの歴史的背景に基づいています。プログラマーは、バグを最小限に抑えるためにテストを書くことが重要だと考えています。Clojureでは、Clojure.testというライブラリを使って、テストを書くことができます。

## 関連リンク：
- https://clojure.org/guides/test_clojure
- https://www.baeldung.com/clojure-test-automation
- https://blog.cleancoder.com/uncle-bob/2017/05/05/TestAssert.html