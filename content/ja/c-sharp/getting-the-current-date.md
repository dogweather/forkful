---
title:                "C#: 「現在の日付を取得する」"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

現在の日付を取得することのメリットは何でしょうか？プログラマーにとって、現在の日付を取得することは重要なタスクの一つです。例えば、ファイルやメールの作成日時や、ユーザーがアプリケーションにアクセスした日時など、様々な場面で必要になるからです。

## 方法

C#では、DateTimeクラスを使用して現在の日付を取得することができます。下記のコードを使用することで、現在の日付をYear-Month-Dayの形式で取得することができます。

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine("現在の日付は{0}です。", currentDate.ToString("yyyy-MM-dd"));
```

出力結果は以下のようになります。

```
現在の日付は2021-07-20です。
```

また、現在の日付だけではなく、曜日や時間など、さまざまな情報を取得することもできます。DateTimeクラスのドキュメントを確認して、自分の必要に応じた情報を取得してみてください。

## ディープダイブ

現在の日付を取得する方法は様々ありますが、実際には内部でどのように処理されているのでしょうか？DateTimeクラスでは、コンピューターの時計情報を使用して現在の日付を取得しています。また、場合によっては時差やサマータイムの影響を受けることもあります。詳細な情報を知りたい方は、DateTimeクラスのドキュメントや関連する記事を参考にしてください。

## 他にも見る

- [DateTimeクラスのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime?view=net-5.0)
- [C#における日付と時間の処理方法について](https://www.atmarkit.co.jp/ait/articles/1710/16/news038.html)