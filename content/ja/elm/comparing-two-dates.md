---
title:                "Elm: 二つの日付を比較する"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

# なぜ日付の比較をするのか

日付の比較は、日々の生活やビジネスにおいて重要な役割を果たします。例えば、特定の期限に遅れているかどうかを判断するために日付の比較を行うことがあります。また、データの整理や分析をする際にも、日付の比較は欠かせない作業です。Elmプログラミングを習得するには、日付の比較方法を理解することが重要です。

## 日付の比較方法

Elmで日付の比較をするには、`Date`モジュールを使用します。以下のコード例では、2つの日付を比較しています。

```Elm
import Date exposing (compare, Date)

date1 = Date.fromTimestamp 1597142400 -- 2020/08/11
date2 = Date.fromTimestamp 1597529400 -- 2020/08/16

comparison = compare date1 date2 -- LT (Less than)
```

上記の例では、2つの日付をそれぞれUnixタイムスタンプとして定義し、`compare`関数を使用して比較しています。結果は`LT`という値が返ってきます。これは、最初の日付が2番目の日付よりも過去であることを示しています。

さらに詳しい日付の比較方法については、`Date`モジュールのドキュメントを参照してください。

## 日付の比較の深層

日付の比較は、単純に前後関係を判断するだけではありません。Elmの`Date`モジュールでは、さまざまなメソッドを使用することで日付の操作が可能です。例えば、`Date.add`を使用することで日付に任意の時間を追加することができます。また、`Date.Subtract`を使用することで日付の差を計算することもできます。

さらに、Elmには日付を操作するための便利なライブラリがたくさんあります。例えば、[billstclair/elm-date-extra](https://package.elm-lang.org/packages/billstclair/elm-date-extra/latest/)や[s-fifteen/elm-date-exploration](https://package.elm-lang.org/packages/s-fifteen/elm-date-exploration/latest/)などがあります。これらのライブラリを使用することで、より柔軟な日付の比較や操作が可能になります。

## 関連リンクを参照

ここまで日付の比較について紹介してきましたが、まだまだElmの日付の操作方法はたくさんあります。ぜひ、以下のリンクを参照して、さらに深い知識を身につけてください。

- [Elm Dateモジュールドキュメント](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [過去の日付を取得する方法](https://discourse.elm-lang.org/t/getting-a-past-date/4743)
- [日付の利用方法を見つける](https://elmprogramming.com/date-usage.html)

### 関連リンク

[もっと詳しく知りたい方におすすめのリンク集]

- [Elm公式サイト](https://elm-lang.org/jp/)
- [Elmパッケージリスト](https://package.elm-lang.org/)