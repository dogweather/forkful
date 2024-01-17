---
title:                "未来や過去の日付の計算"
html_title:           "C#: 未来や過去の日付の計算"
simple_title:         "未来や過去の日付の計算"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

日本語読者のためのC#(最新バージョン)プログラミング記事
非公式トーンと冗長でない、的確なスタイルを使用して書いてください。不必要な単語や文章を避けてください。

## 何か？どうして？

日付を未来や過去に計算することは、ある特定の日付から一定期間を加えたり引いたりすることです。これは、プログラマーが様々なアプリケーションで有用な機能を作成するために使用することができます。

## 方法：

```C#
// 今日の日付を取得
DateTime today = DateTime.Now;
// 10日後の日付を計算
DateTime futureDate = today.AddDays(10);
// 計算した日付を出力
Console.WriteLine("10日後は" + futureDate);
```
出力: 10日後は{計算した日付}

```C#
// このコードは特定の日付から一定期間を引く例です
// 例: 1990年10月10日から5年前の日付を計算
DateTime specificDate = new DateTime(1990, 10, 10);
DateTime pastDate = specificDate.AddYears(-5);
Console.WriteLine("5年前は" + pastDate);
```
出力: 5年前は{計算した日付}

## 深い情報：

日付の計算は非常に古い考え方であり、ローマ時代にさかのぼります。しかし、現在ではさまざまなプログラミング言語やライブラリがこの機能を提供しています。また、別の方法として、timestampを使用して日付を計算することもできます。

## 関連サイト：

- [Microsoft公式ドキュメンテーション](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/datetime/)
- [W3Schoolsでの実践的な例](https://www.w3schools.com/cs/cs_examples.asp)
- [MSDNの日付計算の例](https://docs.microsoft.com/en-us/dotnet/standard/base-types/how-to-calculate-a-date-by-adding-days)