---
title:                "文字列の先頭を大文字にする"
date:                  2024-01-19
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なぜ？とは？)
文字列を大文字にすることは、単純に文字列内の小文字を大文字に変換する操作です。これは、タイトル、見出し、ユーザー名など特定のテキストを強調したい場合や、一貫性のあるデータフォーマットを実現するために行われます。

## How to: (やり方)
```C#
string original = "こんにちは、世界！";
string capitalized = original.ToUpper();

Console.WriteLine(capitalized); // 出力: こんにちは、世界！
```
`.ToUpper()` メソッドを使って簡単に文字列を大文字化できます。このコードは `"こんにちは、世界！"` を出力します。なぜなら、`.ToUpper()` は英字のみを大文字に変換し、日本語のテキストには影響を与えないからです。英語圏のテキストを大文字にする場合は以下のようになります。
```C#
string greeting = "hello, world!";
string capitalizedGreeting = greeting.ToUpper();

Console.WriteLine(capitalizedGreeting); // 出力: HELLO, WORLD!
```

## Deep Dive (深掘り)
`.ToUpper()` は .NET Framework の初期から存在し、文字列内の全ての英字を大文字に変換する最も一般的な方法です。当然ながら、日本語を含む unicode 文字対応していますが、大文字・小文字の区別がない言語には影響を与えません。

`ToUpperInvariant` と `TextInfo.ToUpper` などの代替手段もあります。これらは異なるロケールやカルチャ設定に対応しており、一般的な `.ToUpper()` より柔軟に大文字への変換を行うことができます。

詳細な実装では、拡張メソッドを作成して特定のシナリオやカルチャ固有のルールに合わせることも可能です。 .NET Core および .NET 5.0 以降では、パフォーマンスの向上と共に大文字・小文字変換の正確性が改善されています。

## See Also (関連情報)
- [.NET API Documentation: ToUpper Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=netframework-4.8)
- [.NET API Documentation: ToUpperInvariant Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupperinvariant?view=netframework-4.8)
