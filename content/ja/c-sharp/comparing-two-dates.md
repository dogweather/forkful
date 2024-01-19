---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 标题：C#で日付を比較する方法
## 何でしょうか？なぜですか？
日付の比較は二つの特定の日付の相対的な位置、つまりどちらが早いか遅いかを判断するためのプロセスです。プログラマーはこの処理が必要な理由は色々あり、予定管理、期限切れの検知、経過時間の計算などが含まれます。

## どうやって:
よく使われる日付の比較方法は以下のコードで説明します。

```C#
DateTime date1 = new DateTime(2022, 7, 11);
DateTime date2 = new DateTime(2023, 1, 1);

// -1 means date1 occurs before date2
// 0 means date1 is the same as date2
// 1 means date1 occurs after date2
int result = DateTime.Compare(date1, date2);

Console.WriteLine("Result: {0}", result); // Prints: Result: -1
```
このコンソール出力は `date1`が`date2`より前にあることを示しています。

## 深堀り
より深い歴史的視点から見ると、日付の比較はプログラミングの最初の時から存在しています。しかし、日付の比較によく使われるDateTime.Compare関数は.NETフレームワークのバージョン1.0（2002年）から導入されました。

`DateTime.Compare()`以外にも他の方法があります。それは`DateTime`オブジェクト自体の比較演算子を用いる方法です。このようにコードを書いても同じ結果を得られます。

```C#
DateTime date1 = new DateTime(2022, 7, 11);
DateTime date2 = new DateTime(2023, 1, 1);

bool isEarlier = date1 < date2; // true

Console.WriteLine("Is date1 earlier than date2?: {0}", isEarlier); // Prints: Is date1 earlier than date2?: True
```
## 参照
* [Microsoft公式 DateTime.Compare メソッド ドキュメンテーション](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.compare?view=net-6.0)
* [比較演算子について](https://docs.microsoft.com/ja-jp/dotnet/csharp/language-reference/operators/comparison-operators)