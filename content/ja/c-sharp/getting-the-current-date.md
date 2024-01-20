---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なに？なぜ？

C#で現在の日付を取得するとは、具体的な日付と時刻情報をプログラムで使用することを意味します。これはログの作成、時間のスケジューリングまたは比較など、さまざまな状況で役立つからです。

## どうやって：

以下のコードは、C#で現在の日付と時刻を取得する基本的な方法を示しています：

```C#
DateTime dt = DateTime.Now;

Console.WriteLine("現在の日付と時刻: " + dt);
```

これを実行すると以下のような出力が得られます（出力は実行日時によって異なります）:

```C#
現在の日付と時刻: 2022-01-17 21:48:25
```

## ディープダイブ：

C#で現在の日付と時刻を取得する方法は `DateTime.Now` メソッドを使用するのが一般的です。このメソッドはC#が初めて公開された2002年から存在しています。

他の方法としては、`DateTimeOffset.Now`があります。これは、日付と時刻以外にもUTC（世界協定時刻）からのオフセット（時差）情報も提供します。

内部的には、これらのメソッドはWindowsのシステムAPIを呼び出すことで現在のシステム時刻を取得します。

## 関連情報：

以下のリンクから、関連する情報にアクセスできます：

1. [DateTime.Now Property (Official Microsoft Documentation)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now)
2. [DateTimeOffset.Now Property (Official Microsoft Documentation)](https://docs.microsoft.com/dotnet/api/system.datetimeoffset.now)
3. [UTC and DateTimeOffset (C# Guide)](https://docs.microsoft.com/dotnet/csharp/language-reference/builtin-types/struct#the-datetimeoffset-structure)