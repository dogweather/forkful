---
title:                "今日の日付を取得する"
html_title:           "C#: 今日の日付を取得する"
simple_title:         "今日の日付を取得する"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 今日は何ですか？

現在日付を取得することは、プログラマーがコンピューターに現在の日付を認識させることです。プログラマーは、タイムスタンプや期限付きのタスクを含む多くのアプリケーションで、現在の日付を使用します。

## 方法：

*C#*で現在の日付を取得するには、 *DateTime*クラスを使用します。.NETで提供されるこのクラスには、現在の日付を取得する *Today* メソッドがあります。サンプルコードを以下に示します。

```
DateTime today = DateTime.Today;
Console.WriteLine(today);
```

上記のコードでは、現在の日付が表示されます。以下は出力例です。

```
3/21/2021 12:00:00 AM
```

短い形式で日付を表示したい場合は、*ToString*メソッドを使用します。以下にサンプルコードを示します。

```
DateTime today = DateTime.Today;
Console.WriteLine(today.ToString("d"));
```

上記のコードでは、以下のような出力が得られます。

```
3/21/2021
```

## より詳しく見る：

プログラマーは、現在の日付を取得する以外にも、他の手段で日付を扱うことができます。例えば、 *DateTime* クラスを使用する代わりに、 *DateTimeOffset* クラスを使用することができます。また、日付を取得するだけでなく、日付の比較や操作を行うこともできます。このような様々な方法を覚えておくことで、より効率的に日付を扱うことができるでしょう。

## 関連情報を見る：

* [C# DateTime.Today メソッド](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.today?view=net-5.0)
* [DateTime 説明書き](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
* [DateTimeOffset 説明書き](https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset?view=net-5.0)