---
title:                "Elm: 「2つの日付の比較」"
simple_title:         "「2つの日付の比較」"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

二つの日付を比較することの重要性を理解することは、Elmプログラミングにおいて非常に役立つことです。日付を比較することは、アプリケーションの特定の機能やユーザーのニーズを満たすために必要不可欠な場合があります。この記事では、Elmを使用して二つの日付を比較する方法を学びます。

## 方法

二つの日付を比較するためには、まず日付を処理するためのElmの組み込み関数を使用する必要があります。例えば、 `Date.fromInertval` を使用すると、与えられた日付のオブジェクトを生成することができます。

```Elm
Date.fromInterval { start = Date.initial, end = Date.now }
```

この組み込み関数を使用すると、 `start` と `end` の間の日付を含むオブジェクトを取得できます。もし比較する二つの日付が文字列で与えられる場合は、 `Date.fromString` を使用してオブジェクトに変換すれば良いでしょう。

実際に二つの日付を比較する場合は、再度組み込み関数 `Date.comparableDate` を使用します。この関数を使用すると、二つの日付を比較可能な値に変換することができます。

```Elm
Date.comparableDate (Date.fromString "2020-01-01") == Date.comparableDate (Date.fromString "2020-02-01")
-- 結果: True
```

## 詳細を深める

Elmでは、日付を比較する際には大局的な観点から事を見ることに意味があります。例えば、繰り返し日付を処理する場合には、それぞれの日付が他の日付に影響を与えないように注意する必要があります。また、様々なタイムゾーンを扱う際にも注意が必要です。これらの問題を回避するために、日付を比較する際にはElmの組み込み関数をよく理解し、適切に使用することが重要です。

## 参考リンク

- [Elmドキュメンテーション: 日付](https://package.elm-lang.org/packages/elm/time/latest/) 
- [JavaScriptの日付を比較する方法](https://www.w3schools.com/js/js_dates.asp)
- [ElmのDateライブラリの使い方の基本](https://qiita.com/takeyuichi/items/89ee4e7887d1eb388551) 

## 他に読みたい

- [Elmで日付をフォーマットする方法](https://medium.com/@clarencetmesplanation/take-control-of-your-date-format-in-elm-ac5ee40f9cac)
- [Elmでタイムゾーンを処理する方法](https://medium.com/@jessitron/time-zones-in-elm-86f3d4696a6)