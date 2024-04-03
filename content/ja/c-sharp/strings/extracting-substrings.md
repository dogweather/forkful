---
date: 2024-01-20 17:45:09.744723-07:00
description: "How to: (\u3084\u308A\u65B9) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.108278-06:00'
model: gpt-4-1106-preview
summary: .
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
