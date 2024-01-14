---
title:    "Haskell: 二つの日付の比較"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## なぜ

日付を比較することの重要性は、プログラムやアプリケーションで日付を使用する必要があるからです。例えば、催事の開始日と終了日を比較して、催事がどれくらい続くのかを計算したり、期限を過ぎたタスクを特定したりすることができます。日付の比較は、様々なタイプのプログラミングにおいて重要なスキルです。

## ハウツー

まずはHaskellで日付を比較するための基本的なコードを見てみましょう。

```Haskell
import Data.Time
import Data.Maybe

date1 :: Maybe Day
date1 = parseTimeM True defaultTimeLocale "%Y-%m-%d" "2020-01-01"

date2 :: Maybe Day
date2 = parseTimeM True defaultTimeLocale "%Y-%m-%d" "2020-01-05"

compareDates :: Maybe Ordering
compareDates = compare <$> date1 <*> date2
```

上記のコードでは、日付を可能な限り安全に扱うために、Data.TimeモジュールからparseTimeM関数を使用しています。また、Maybe型を使用することで、日付が正しく解析されなかった場合にも安全に処理することができます。

さらに、compareDates関数では、日付を比較するためにHaskellの標準関数であるcompareを使用しています。この関数は、2つのMaybe値を受け取り、その両方がJust値である場合にのみ比較を行うことができます。

コードを実行すると、compareDates関数の結果であるMaybe Ordering型が表示されます。Ordering型には、LT（日付1が日付2より前）、GT（日付1が日付2より後）、EQ（日付1と日付2が同じ）の3つの値が含まれています。このようにして、日付の比較結果をプログラム内で使用することができます。

## ディープダイブ

Haskellでは、 Date型やDateTime型など、さまざまな日付型が提供されています。これらの型は、日付を表すための便利なメソッドや関数を持っています。例えば、日付の加算や減算、フォーマットの変更などができます。さらに、比較ではなく等価性を確認するための関数も提供されています。

また、Haskellの標準ライブラリではないものの、Hackage上には日付を効率的に比較するためのさまざまなライブラリもあります。もしもっと高度な日付比較が必要な場合は、そのようなライブラリを使用することもできます。

## その他

Haskellで日付を比較する方法や、さまざまな日付型について学びたい場合は、以下のリンクを参考にしてください。

- [HaskellのData.Timeモジュール](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Hackageの日付比較ライブラリの一覧](https://hackage.haskell.org/packages/search?terms=date+comparison)