---
title:                "Clojure: 日付を文字列に変換する"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

今日はシンプルなClojureのプログラミングについて話します。具体的には、日付を文字列に変換する方法について説明します。日付を文字列に変換する理由は、時には必要になるからです。例えば、データベースに日付を保存する際や、プログラム内で日付を扱う必要がある場合などです。

## なぜ？

日付を文字列に変換することのメリットは、プログラムで日付を簡単に扱えるようになることです。また、特定のフォーマットに日付を合わせることができるため、データの整理や比較をしやすくなります。

## 方法

日付を文字列に変換するには、`#inst`関数を使用します。例えば、次のコードは今日の日付を文字列で出力します。

```Clojure
(def today (java.util.Date.))
#inst "2019-06-26T13:00:00.000-00:00"
```

日付を特定のフォーマットに変換するには、`#date`関数を使用します。例えば、次のコードは今日の日付を「月/日/年」の形式で出力します。

```Clojure
(def today (java.util.Date.))
#date "06/26/2019"
```

また、さまざまな日付情報を文字列に変換するには、`clj-time`ライブラリを使用することもできます。例えば、次のコードは現在の日時をフォーマットした文字列で出力します。

```Clojure
(require '[clj-time.core :as t])

(def now (t/now))
(format (t/formatter "yyyy/MM/dd hh:mm:ss") now)
"2019/06/26 22:00:00"
```

## ディープダイブ

日付を文字列に変換する際に注意する点は、日付のフォーマットによっては結果が想定外になる可能性があることです。また、タイムゾーンの扱いにも注意が必要です。詳しくは、公式ドキュメントやユーザーコミュニティの情報を参考にしてください。

## 関連リンク

* [Clojure 公式ドキュメント](https://clojure.org/)
* [clj-time ライブラリ](https://github.com/clj-time/clj-time)
* [Clojure ユーザーグループ](https://groups.google.com/forum/#!forum/clojure-ja)