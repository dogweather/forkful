---
title:    "Clojure: 現在の日付の取得"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
Clojureで現在の日付を取得するには、さまざまな理由があります。例えば、プログラムで現在の日付を使用したい場合や、日付を使用して他の機能を作成したい場合などが挙げられます。

## 方法
現在の日付を取得する一番簡単な方法は、`java.util.Calendar`クラスを使用することです。以下のコードブロックを参考にしてください。

```Clojure
(let [date (java.util.Calendar/getInstance)]
  (println (.getTime date)))
```

上記のコードでは、`java.util.Calendar`クラスの`getInstance`メソッドを使用して、現在の日付を取得しています。そして、`.getTime`メソッドを使用して、日付を出力しています。

## ディープダイブ
上記の方法以外にも、Clojureで現在の日付を取得する方法はいくつかあります。例えば、`java.time.LocalDate`クラスや`java.util.Date`クラスを使用する方法などがあります。詳細な情報は、[こちらのリンク](https://clojuredocs.org/clojure.java-time/now)を参考にしてください。

## もっと見る
- [Clojureで日付を操作するための便利なライブラリ](https://www.mizugorou.net/post/2013/07/09/clojure-datetime-libraries/)
- [日付操作に必要なJavaクラス一覧](https://docs.oracle.com/javase/jp/10/docs/api/java/util/package-summary.html)