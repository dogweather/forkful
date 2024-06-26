---
date: 2024-01-20 17:45:09.744723-07:00
description: "How to: (\u3084\u308A\u65B9) \u7C21\u5358\u3067\u3059\u306D\u3002`Substring`\u30E1\
  \u30BD\u30C3\u30C9\u306F\u3001\u6587\u5B57\u5217\u304B\u3089\u5FC5\u8981\u306A\u90E8\
  \u5206\u3092\u5207\u308A\u51FA\u3059\u306E\u306B\u4F7F\u3044\u307E\u3059\u3002\u4F8B\
  \u306F\u3001\"\u3053\u3093\u306B\u3061\u306F\u3001\u4E16\u754C!\"\u304B\u3089\"\u4E16\
  \"\u3092\u62BD\u51FA\u3057\u3066\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.985581-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u7C21\u5358\u3067\u3059\u306D\u3002`Substring`\u30E1\
  \u30BD\u30C3\u30C9\u306F\u3001\u6587\u5B57\u5217\u304B\u3089\u5FC5\u8981\u306A\u90E8\
  \u5206\u3092\u5207\u308A\u51FA\u3059\u306E\u306B\u4F7F\u3044\u307E\u3059\u3002\u4F8B\
  \u306F\u3001\"\u3053\u3093\u306B\u3061\u306F\u3001\u4E16\u754C!\"\u304B\u3089\"\u4E16\
  \"\u3092\u62BD\u51FA\u3057\u3066\u3044\u307E\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

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
