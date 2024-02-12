---
title:                "文字列の連結"
aliases: - /ja/c-sharp/concatenating-strings.md
date:                  2024-01-20T17:34:30.987909-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の結合とは複数の文字列を一つにすることです。プログラマーは文章をダイナミックに作成したり、データを組み合わせたりする際に利用します。

## How to: (方法)
```C#
string hello = "こんにちは、";
string world = "世界！";
string greeting = hello + world; // 文字列の結合
Console.WriteLine(greeting); // 出力：こんにちは、世界！
```
上記は最も基本的な結合方法です。`String.Concat` や `StringBuilder` も使えます。
```C#
string hello = "こんにちは、";
string world = "世界！";
string greeting = String.Concat(hello, world); // String.Concatを使った結合
Console.WriteLine(greeting); // 出力：こんにちは、世界！

var builder = new StringBuilder();
builder.Append(hello).Append(world); // StringBuilderを使った結合
Console.WriteLine(builder.ToString()); // 出力：こんにちは、世界！
```

## Deep Dive (掘り下げ)
歴史的に、文字列の結合は `+` 演算子で行われてきましたが、多数の結合が必要な場合にはパフォーマンスが低下します。なぜなら、C#の `string` はイミュータブル（不変）なので、結合のたびに新しい文字列がメモリに作成されるからです。その点で `String.Concat` は効率的ですが、最も効率が良いのは `StringBuilder` の使用です。これは内部でバッファを管理し、不必要なメモリ割り当てを減らすことで、結合処理を高速化します。

代替方法としては、C# 6.0から導入された文字列補間もあります。これはコードをより読みやすく、メンテナンスしやすくするためのものです。
```C#
string name = "太郎";
string message = $"こんにちは、{name}さん！"; // 文字列補間
Console.WriteLine(message); // 出力：こんにちは、太郎さん！
```

## See Also (関連情報)
- [String.Concat Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat)
- [StringBuilder Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder)
- [C# 文字列補間](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- [C# 文字列とその操作](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/strings/)
