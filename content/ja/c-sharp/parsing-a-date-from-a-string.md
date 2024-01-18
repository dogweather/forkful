---
title:                "文字列から日付を解析する"
html_title:           "C#: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# What & Why?

パースとは、文字列から日付を抽出することを指します。プログラマーは、日付を文字列から抽出することで、日付を整理し、処理することができます。

# How to:

```C#
DateTime date = DateTime.Parse("10/31/2021");
Console.WriteLine(date);
```

このコードでは、文字列 "10/31/2021" がパースされ、変数 "date" に日付として格納されます。コンソールには、2021年10月31日という形式で出力されます。

# Deep Dive:

日付のパースには、C#において便利な "Parse" メソッドが用意されています。このメソッドは、DateTimeオブジェクトに変換するための様々なオプションがあります。また、文字列の形式に合わせて、パターンを指定することもできます。

もし、日付のパターンが特定の場合は、"TryParseExact" メソッドを使用することで、より正確なパースが可能です。このメソッドでは、日付のパターンを指定することで、ソフトウェアにとって意図しない形式の日付が入力されても、エラーを発生させることなく、適切にパースすることができます。

他のプログラミング言語にも、日付のパースに使えるメソッドやライブラリが存在しますが、C#の "Parse" メソッドは、手軽で使いやすく、多くのオプションを提供しています。

# See Also:

関連情報は以下を参照してください。

- C#の日付のパースについて: https://docs.microsoft.com/ja-jp/dotnet/csharp/programming­guide/strings/#parseformat
- 日付時刻を扱う際のベストプラクティス: https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/best-practices-datetime
- 日付のパースについての詳細: https://devblogs.microsoft.com/dotnet/strings-and-dates/