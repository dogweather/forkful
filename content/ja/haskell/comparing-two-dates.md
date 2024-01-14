---
title:                "Haskell: 「日付の比較」"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 方針
日本の読者の皆様、こんにちは！今日は、Haskellで日付を比較する方法についてお話ししたいと思います。日付の比較は、日付を処理するプログラムを開発する際に非常に重要なスキルです。どのようにすれば、Haskellを使って日付を比較することができるのでしょうか？それでは、さっそく見ていきましょう！

## 方法
まず最初に、Haskellで日付を表現する方法を学びましょう。Haskellでは、```UTCTime```という型を使って日付を表します。例えば、2020年4月1日を表す場合、以下のように記述します。

```Haskell
let date = UTCTime (fromGregorian 2020 4 1) (secondsToDiffTime 0)
```

上記のコードでは、```UTCTime```コンストラクタを使い、```fromGregorian```関数を使って日付を指定しています。```secondsToDiffTime```関数を使うことで、時間を表すための型を指定しています。

さて、日付を比較するには、比較演算子を使用します。Haskellでは、```Ord```型クラスを使って比較が可能な型を表現します。```UTCTime```型も```Ord```型クラスに属しているため、比較演算子を使うことができます。

例えば、以下のように日付を比較することができます。

```Haskell
date1 > date2 -- date1がdate2よりも未来の日付かどうかを判定
date1 == date2 -- date1とdate2が同じ日付かどうかを判定
date1 >= date2 -- date1がdate2よりも未来の日付かまたは同じ日付かどうかを判定
```

ここまで、日付を比較するための基本的な方法を学びました。次に、さらに深く掘り下げていきましょう。

## 深堀り
Haskellでは、日付を比較するための多くの便利な関数が提供されています。例えば、```diffUTCTime```関数を使うことで、２つの日付の差を取得することができます。また、```addUTCTime```関数を使うことで、指定した時間を日付に加算することができます。

さらに、Haskellでは日付の比較ができるだけでなく、日付をソートすることも可能です。```sort```関数を使うことで、日付のリストをソートすることができます。

さらに、日付をフォーマットするための関数も提供されています。例えば、```formatTime```関数を使うと、指定したフォーマットで日付を表示することができます。

以上で、Haskellで日付を比較する方法の紹介を終えたいと思います。みなさんもぜひ、これらの関数を使って日付を扱うプログラムを開発してみてください！

## 参考リンク
- [Haskellの日付処理ドキュメント](https://www.haskell.org/haskellwiki/Time_and_Date)
- [日付や時間を扱うための基本パッケージのドキュメント](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [日付をフォーマットするための関数](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html)