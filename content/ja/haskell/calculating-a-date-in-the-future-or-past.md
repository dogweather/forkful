---
title:                "「未来または過去の日付の計算」"
html_title:           "Haskell: 「未来または過去の日付の計算」"
simple_title:         "「未来または過去の日付の計算」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何 & なぜ?

「未来や過去の日付を計算すること」とは、指定された日付から一定期間後や前の日付を求めることです。プログラマーがこれを行う理由は、アプリケーションにおいて特定の日付を操作する必要があるためです。

## 方法：

以下のコードブロックに、Haskellを使って未来や過去の日付を計算する方法を示します。

```Haskell
-- 未来の日付を計算する
let future = addDays 30 today
-- 過去の日付を計算する
let past = addDays (-30) today
```

上記のコードを実行すると、現在の日付から30日後の日付が`future`に、30日前の日付が`past`に格納されます。

## 探究

### 歴史的背景

過去において、日付の計算は非常に困難な作業と考えられていました。しかし、近年のプログラミング言語の発展により、日付の計算はより簡単に行えるようになりました。

### 代替手段

未来や過去の日付を計算するための代替手段としては、他のプログラミング言語でも同様の機能が備わっている場合があります。例えば、JavaやPythonなどでも同様の手法で日付の計算が可能です。

### 実装の詳細

Haskellにおいて、日付の計算は標準ライブラリの`Data.Time`モジュールを使用して行います。`addDays`関数を使用することで、指定した日付から任意の日数を加算または減算することができます。

## 関連情報

- [Haskell Data.Time モジュールのドキュメント](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Java での日付の計算方法の例](https://www.geeksforgeeks.org/java-util-date-class-java/)