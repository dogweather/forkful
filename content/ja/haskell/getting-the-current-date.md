---
title:    "Haskell: 現在の日付を取得する"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 今日はなぜ日付を取得する必要があるのか

日付を現在のローカル時間で取得することは、プログラミングでよく必要とされます。特定のタスクやイベントを実行した際に、現在の時間や日付を知ることは重要です。

## 日付を取得する方法

Haskellでは、Data.Timeモジュールを使用して現在の日付を取得することができます。以下のコードを参考にしてください。

```Haskell
import Data.Time

main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

上記のコードでは、Data.TimeモジュールのgetCurrentTime関数を使用して現在の日付を取得し、utctDay関数を使用して日付部分のみを取り出し、その結果をprint関数を使用して出力しています。

実行すると、以下のような出力が得られます。

```
2019-10-30
```

## 日付を取得する仕組みの詳細

Haskellでは、様々なフォーマットの日付や時間を扱うための様々な関数や型が用意されています。例えば、LocalTime型やUTCTime型などがあり、それぞれ異なる方法で日付や時間を表現しています。

また、Data.Timeモジュールにはさらに詳細な関数が用意されており、タイムゾーンの変更や特定の日付形式への変換などが可能です。

## See Also

- [Data.Timeモジュールのドキュメント](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Haskellで日付を扱う方法](https://qiita.com/waizu0/items/be116ae6910f82c627e6)
- [Haskell入門: 関数と型](https://www.datacamp.com/community/tutorials/tutorial-functions-types-haskell)