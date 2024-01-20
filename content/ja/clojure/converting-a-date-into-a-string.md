---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？ (What & Why?)

日付を文字列に変換するとは、特定の日付を単なる文字情報に変換することを意味します。プログラマーは日付データを人間が理解しやすいフォーマットで表示するためや、独自の日付フォーマットを作成するためにこれを行います。

## ハウツー (How to):

Clojureで日付を文字列に変換する簡単な例を以下に示します。

```clojure
(require '[clj-time.format :as f])

(def date-format (f/formatter "yyyy年MM月dd日"))

(defn date-to-string [date]
  (f/unparse date-format date))
```

これにより、次のような出力が得られます。

```clojure
(date-to-string (time/now))
; => "2022年02月03日"
```

このコードでは、まず`clj-time.format`というライブラリをインポートし、次に所望の日付フォーマットを定義しています。このフォーマットを使ってある日付を文字列に変換する関数を定義します。

## ディープダイブ (Deep Dive)

古代から人々は日付を表現するための様々な方法を考え出してきました。今日では多くのシステムでISO 8601という日付表現の標準が使われていますが、それぞれの国や地域、産業によっては異なる日付表現が用いられます。

Clojureでは`clj-time.format`ライブラリの`formatter`関数を使って日付を文字列に変換します。しかし、Javaの`SimpleDateFormat`クラスも利用可能で、より多くのオプションが利用できます。

内部的には、日付は特定の瞬間を表現するための一連の数字として格納されています。日付を文字列に変換するというのは、この数字を人間が理解可能な形に変換する作業と言えます。

## 参考リンク (See Also)

- Clojureの日付と時間について詳しく学ぶ: https://clojuredocs.org/clojure.core/date
- `clj-time.format`ライブラリのドキュメンテーション: https://clojars.org/clj-time
- ISO 8601について詳しく理解する: https://www.iso.org/iso-8601-date-and-time-format.html
- Javaの`SimpleDateFormat`クラスについて: https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html.