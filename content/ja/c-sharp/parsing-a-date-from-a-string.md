---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付のパースとは文字列から日付を抽出することを指します。この操作は、受け取ったデータを特定のフォーマット（通常は日付）に変換するためにプログラマーによって実行されます。

## 方法:

以下に、C#で文字列から日付を解析する方法を示します:

```C#
using System;

// 日付を格納する文字列
string dateString = "2021-10-15";

// 変換された日付を格納するDateTimeオブジェクト
DateTime parsedDate = DateTime.Parse(dateString);

// 出力
Console.WriteLine(parsedDate);
```

これは次のように出力されます:

```
2021-10-15 12:00:00 AM
```

## より深く:

日付の解析には歴史的な文脈があり、元々は文字列操作とパターンマッチングに依存していました。しかしC#では便利な関数`DateTime.Parse`が提供されており、これを使用するだけで解析を行うことが可能です。

この操作の代替手段としては、`DateTime.TryParse`や`DateTime.ParseExact`があります。これらのメソッドは日付の書式を指定したり、解析が可能かどうかを確認したりするために使われます。

実装の詳細については、`DateTime.Parse`は内部的に`DateTime.TryParse`を使用しており、解析に失敗した場合にはInvalidCastExceptionをスローします。

## 関連資料:

以下のリンクは、日付の解析に関してさらに詳しい情報を提供しています:

- [Microsoft Docs: DateTime.Parse メソッド (System)](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.parse?view=net-5.0)
- [Microsoft Docs: DateTime.TryParse メソッド (System)](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.tryparse?view=net-5.0)
- [Microsoft Docs: DateTime.ParseExact メソッド (System)](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.parseexact?view=net-5.0)