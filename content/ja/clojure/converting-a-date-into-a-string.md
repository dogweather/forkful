---
title:                "日付を文字列に変換する"
html_title:           "Clojure: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##なぜ

日付を文字列に変換することへの関心は、日付データを扱うプログラムにおいて必須の機能です。日付を文字列に変換することにより、人間が読みやすい形式にデータを表示することができます。

##やり方

日付を文字列に変換するには、Clojureの「format」関数を使用します。この関数を使用するためには、日付のフォーマットを指定する必要があります。例えば、MM/dd/yyyyの形式で日付を表示する場合は、「"MM/dd/yyyy"」というフォーマット文字列を指定します。

```Clojure
(format "MM/dd/yyyy" (java.util.Date.))
;;=> "12/14/2021"
```

また、Clojureには「clj-time」というライブラリがあり、日付と時間の操作を行うための便利な関数を提供しています。このライブラリを使うことで、より柔軟な日付の変換が可能になります。

```Clojure
(require '[clj-time.format :as fmt])
(fmt/unparse (fmt/formatters :date) (java.util.Date.))
;;=> "Tuesday, December 14, 2021"
```

##深層探究

日付を文字列に変換するには、日付を表すデータ型をどのように処理するかという点が重要になります。Clojureでは、内部的にはJavaの「java.util.Date」クラスを使用して日付を表します。そのため、日付を文字列に変換する際にはJavaのフォーマット関数を使用することになります。

Clojureでは「clj-time.format」ライブラリを使用することで、より柔軟なフォーマット指定が可能になります。また、日付の操作にも便利な関数が提供されていますので、日付を扱うプログラムをより簡単に実装することができます。

##参考リンク

- [Clojure format](https://clojuredocs.org/clojure.core/format) (公式ドキュメント)
- [clj-time library](https://github.com/clj-time/clj-time) (公式GitHubページ)
- [Clojure で日付と時間を扱おう (clj-time の使い方) | Mochi Mochi Blog](https://mochi-mochi.github.io/2015/07/04/training-date-time-of-clojure-2.html) (チュートリアル記事)