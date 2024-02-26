---
date: 2024-01-20 17:45:09.744723-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u90E8\u5206\u6587\u5B57\u5217\u3092\u62BD\
  \u51FA\u3059\u308B\u3053\u3068\u306F\u3001\u7279\u5B9A\u306E\u7BC4\u56F2\u306E\u6587\
  \u5B57\u3092\u53D6\u5F97\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u3053\
  \u308C\u3092\u884C\u3046\u7406\u7531\u306F\u3001\u30C7\u30FC\u30BF\u51E6\u7406\u3001\
  \u30E6\u30FC\u30B6\u5165\u529B\u306E\u89E3\u6790\u3001\u307E\u305F\u306F\u7279\u5B9A\
  \u306E\u60C5\u5831\u3092\u8868\u793A\u3059\u308B\u969B\u306B\u5FC5\u8981\u3060\u304B\
  \u3089\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.127595-07:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u90E8\u5206\u6587\u5B57\u5217\u3092\u62BD\
  \u51FA\u3059\u308B\u3053\u3068\u306F\u3001\u7279\u5B9A\u306E\u7BC4\u56F2\u306E\u6587\
  \u5B57\u3092\u53D6\u5F97\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u3053\
  \u308C\u3092\u884C\u3046\u7406\u7531\u306F\u3001\u30C7\u30FC\u30BF\u51E6\u7406\u3001\
  \u30E6\u30FC\u30B6\u5165\u529B\u306E\u89E3\u6790\u3001\u307E\u305F\u306F\u7279\u5B9A\
  \u306E\u60C5\u5831\u3092\u8868\u793A\u3059\u308B\u969B\u306B\u5FC5\u8981\u3060\u304B\
  \u3089\u3067\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列から部分文字列を抽出することは、特定の範囲の文字を取得するプロセスです。これを行う理由は、データ処理、ユーザ入力の解析、または特定の情報を表示する際に必要だからです。

## How to: (やり方)
```C#
string fullString = "こんにちは、世界!";
int startIndex = 5;
int length = 2;

string substring = fullString.Substring(startIndex, length);

Console.WriteLine(substring); // 出力: 世
```

簡単ですね。`Substring`メソッドは、文字列から必要な部分を切り出すのに使います。例は、"こんにちは、世界!"から"世"を抽出しています。

## Deep Dive (深掘り)
文字列の部分抽出は、最初のプログラミング言語が開発された時から存在しています。C#での`Substring`メソッドは他の言語の似た機能を踏襲していますが、.NET の `Span<T>` や `Memory<T>` などの新しい型も同様の操作に使えます。これにより、パフォーマンスを向上させることができます。また、Linq を使った方法や、`Regex` を使って複雑なパターンの部分文字列を抽出することもできます。

## See Also (関連情報)
- [.NET の `Substring` メソッドのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.substring)
- [C# での `Span<T>` の使い方](https://docs.microsoft.com/ja-jp/dotnet/api/system.span-1)
- [C# の `Regex` クラスの使用方法](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/regular-expression-language-quick-reference)
