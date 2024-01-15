---
title:                "文字列を小文字に変換する"
html_title:           "C#: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換することについて、なぜ誰かが取り組む必要があるのかを最大2つの文章で説明します。

## 方法
以下に、文字列を小文字に変換するためのC#のコーディング例とサンプル出力のあるコードブロックを示します。

```C#
// 文字列を小文字に変換する方法
string name = "JOHN";
name = name.ToLower();
Console.WriteLine(name);
// Output: john
```

## 深堀り
文字列を小文字に変換するためのより詳細な情報を提供します。文字列は、Unicode文字に基づいてパフォーマントするため、文字列を小文字に変換する方法は多数あります。また、特定のカルチャーに基づいて変換することも可能です。

## See Also
- [String.ToLower メソッド (C# リファレンス)](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.tolower?view=netcore-3.1)
- [Unicodeでの大文字と小文字の概要](https://docs.microsoft.com/ja-jp/windows/win32/intl/unicode-case-mapping)
- [カルチャー固有の情報を使用するための .NET API](https://docs.microsoft.com/ja-jp/dotnet/core/internationalization/)