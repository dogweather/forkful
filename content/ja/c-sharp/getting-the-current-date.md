---
title:                "C#: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

こんにちは、C#プログラマーの皆さん。今日は皆さんに、現在の日付を取得するプログラムを紹介したいと思います。なぜこのようなプログラムを作成するのか、そして具体的にどのようにするのか、そしてさらに深く掘り下げる前に、まず最初にその理由をお伝えします。

## なぜ？

現在の日付を取得するには、多くの理由が挙げられます。例えば、タイムスタンプ付きのログを作成する必要がある場合や、アプリケーション内で日付を表示する必要がある場合などです。また、特定の作業を行う前に、現在の日付を確認しておくことが重要な場合もあります。いずれにせよ、日付取得の機能は、多くのプログラムで必要不可欠なものです。

## 作り方

今回は、C#の標準ライブラリであるDateTimeクラスを使用して、現在の日付を取得する方法を紹介します。以下のコードを参考にしてください。

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate);
```

上記のコードを実行すると、現在の日付が以下のような形式で出力されます。

```bash
21/07/2021 13:30:00
```

もし、特定の形式で日付を表示したい場合は、以下のように書式指定してください。

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate.ToString("dd/MM/yyyy"));
```

このように指定することで、日付の形式をカスタマイズすることができます。

## 深堀り

DateTimeクラスは、現在の日付を取得するだけでなく、様々な日付操作を行うことができます。例えば、2つの日付を比較して、どちらが前の日付かを判定することができます。また、指定した日付に対して、特定の時間を加算したり減算したりすることもできます。詳しくは、[公式ドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime?view=net-5.0)を参考にしてください。

## おすすめのリンク

- [DateTimeクラスのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime?view=net-5.0)
- [DateTimeのチートシート](https://devhints.io/datetime)
- [C# DateTimeを使った日付操作の基本](https://www.sejuku.net/blog/33848)
- [C#で日付・時刻処理をお手軽に行う方法](https://www.buildinsider.net/web/bookdotnet/020301) 

**詳しくはこちらをご覧ください！**

---
見出し： おすすめのリンク

 - [DateTimeクラスのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime?view=net-5.0)
 - [DateTimeのチートシート](https://devhints.io/datetime)
 - [C# DateTimeを使った日付操作の基本](https://www.sejuku.net/blog/33848)
 - [C#で日付・時刻処理をお手軽に行う方法](https://www.buildinsider.net/web/bookdotnet/020301)