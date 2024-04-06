---
date: 2024-01-20 17:34:30.987909-07:00
description: "How to: (\u65B9\u6CD5) \u4E0A\u8A18\u306F\u6700\u3082\u57FA\u672C\u7684\
  \u306A\u7D50\u5408\u65B9\u6CD5\u3067\u3059\u3002`String.Concat` \u3084 `StringBuilder`\
  \ \u3082\u4F7F\u3048\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.989130-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u4E0A\u8A18\u306F\u6700\u3082\u57FA\u672C\u7684\u306A\u7D50\
  \u5408\u65B9\u6CD5\u3067\u3059\u3002`String.Concat` \u3084 `StringBuilder` \u3082\
  \u4F7F\u3048\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

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
