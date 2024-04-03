---
date: 2024-01-20 17:34:30.987909-07:00
description: "\u6587\u5B57\u5217\u306E\u7D50\u5408\u3068\u306F\u8907\u6570\u306E\u6587\
  \u5B57\u5217\u3092\u4E00\u3064\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u6587\u7AE0\u3092\u30C0\u30A4\u30CA\u30DF\u30C3\
  \u30AF\u306B\u4F5C\u6210\u3057\u305F\u308A\u3001\u30C7\u30FC\u30BF\u3092\u7D44\u307F\
  \u5408\u308F\u305B\u305F\u308A\u3059\u308B\u969B\u306B\u5229\u7528\u3057\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.110754-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u7D50\u5408\u3068\u306F\u8907\u6570\u306E\u6587\
  \u5B57\u5217\u3092\u4E00\u3064\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u6587\u7AE0\u3092\u30C0\u30A4\u30CA\u30DF\u30C3\
  \u30AF\u306B\u4F5C\u6210\u3057\u305F\u308A\u3001\u30C7\u30FC\u30BF\u3092\u7D44\u307F\
  \u5408\u308F\u305B\u305F\u308A\u3059\u308B\u969B\u306B\u5229\u7528\u3057\u307E\u3059\
  \u3002."
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
