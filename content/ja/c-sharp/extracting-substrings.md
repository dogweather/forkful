---
title:                "部分文字列の抽出"
date:                  2024-01-20T17:45:09.744723-07:00
model:                 gpt-4-1106-preview
simple_title:         "部分文字列の抽出"

category:             "C#"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/extracting-substrings.md"
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
