---
title:    "Clojure: 日付を文字列に変換する"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換するのは、プログラミングで重要なタスクです。例えば、データベースに格納されている日付を表示する必要がある場合や、特定の形式の日付を扱う必要がある場合などに、日付を文字列に変換する必要があります。

## 方法

Clojureでは、日付を文字列に変換するためには、組み込みの`str`関数を使用します。以下のコード例を参考にしてください。

```Clojure
(str 2020 1 1)
```

上記のコードを実行すると、`2020 1 1`という日付が文字列`"20200101"`に変換されます。

日付に対してフォーマットを適用する場合は、`format`関数を使用します。例えば、`dd/MM/yyyy`というフォーマットを適用する場合は以下のようになります。

```Clojure
(format "%1$td/%1$tm/%1$tY" (java.util.Date.))
```

このコードを実行すると、現在の日付が`"01/01/2020"`の形式で文字列に変換されます。

## 深堀り

Clojureでは、`java.util.Date`クラスや`java.time`パッケージを使用して日付を取り扱います。詳細な情報は、ClojureのドキュメントやJavaの公式ドキュメントを参照してください。

## 参考リンク

- [Clojure 公式ドキュメント](https://clojure.org/)
- [Java 公式ドキュメント](https://www.oracle.com/java/technologies/javase-documentation.html)