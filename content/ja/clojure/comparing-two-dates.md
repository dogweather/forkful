---
title:    "Clojure: 「2つの日付の比較」"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

# なぜ

プログラマーにとって、日付を比較することは非常に重要です。日付はシステムで重要な役割を果たし、データの整理や検索、分析に欠かせません。日付の比較は、データの正確性や取り扱い方に大きな影響を与えるため、正確な方法で行う必要があります。

# 方法

日付を比較する方法は多数ありますが、今回はClojureを使用して2つの日付を比較する方法を紹介します。Clojureは、関数型プログラミング言語であり、コードがシンプルかつ読みやすいため、日付の比較にも最適です。

まずは、clojure.coreライブラリの比較関数を使用して、日付の大小を比較してみましょう。以下のコードを試してみてください。

```Clojure
(require '[clojure.core :as c])

(def date1 (c/local-date 2021 5 20))
(def date2 (c/local-date 2021 5 25))

(c/compare date1 date2) ; 結果は-1
```

このコードでは、2021年5月20日を表すdate1と2021年5月25日を表すdate2を定義し、c/compare関数を使用して日付の大小を比較しています。結果は-1となります。この結果から、date1がdate2より前の日付であることがわかります。

次に、clojure.java-timeライブラリを使用して、より詳細な日付の比較を行ってみましょう。以下のコードを試してみてください。

```Clojure
(require '[java-time :as t])

(def date1 (t/local 2019 10 10))
(def date2 (t/local 2019 10 10))

(t/period-between date1 date2) ; 結果は[0 0 0]
```

このコードでは、date1とdate2が同じ日付であることを表すため、結果は[0 0 0]となります。しかし、日付を変更して実行すると、[0 0 1]や[0 1 0]など、期間の差が表示されます。

# ディープダイブ

日付を比較する際には、タイムゾーンの違いや夏時間の影響など、さまざまな要因に注意する必要があります。例えば、UTCのサマータイム (夏時間) は、日付の比較に影響を与える可能性があります。また、日付を表すデータ型の選択によっても、比較の結果に差が出ることがあります。

さらに詳しい情報を得るには、Clojureの公式ドキュメントや関連する記事を参考にしてください。日付の取り扱いには、多くの注意が必要ですが、適切な方法でコーディングすることで、正確な結果を得ることができます。

# 関連記事

- [Clojure公式ドキュメント](https://clojure.org/)
- [Java-Timeライブラリ](https://github.com/dm3/clojure.java-time)
- [日付の比較についての詳しい記事](https://clojure.org/guides/juhistic/clojure_dates)