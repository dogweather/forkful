---
aliases:
- /ja/c-sharp/finding-the-length-of-a-string/
date: 2024-01-20 17:47:01.839401-07:00
description: ''
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.909464
model: gpt-4-1106-preview
summary: ''
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
---

{{< edit_this_page >}}

## What & Why?
## 何となぜ？
文字列の長さを見つけるというのは、単純に文字列が何文字から成っているかを数えることです。この操作は、入力の検証、データの整形、ループでの処理など、さまざまな状況で役立ちます。

## How to:
## 方法：
```C#
using System;

class Program
{
    static void Main()
    {
        string example = "こんにちは";
        int length = example.Length;

        Console.WriteLine("String length: " + length);
    }
}
```
Sample Output:
```
String length: 5
```
文字列の`Length`プロパティを使って、文字数を簡単に取得できます。

## Deep Dive
## 詳細な解説：
C#では、`string`クラスには`.Length`プロパティが備わっている。これは文字列に含まれる文字の数を返す。

歴史的に見ると、C言語などでは文字列は文字の配列として扱われ、ヌル文字('\0')で終わる。そのため、文字列の長さを見つけるには配列をループし、ヌル文字に到達するまでカウントする必要があった。C#では`.Length`がシンプルにその仕事をしてくれる。

代替方法として、LINQ拡張メソッド`Count()`も持つことができるが、`Length`プロパティの方が直接的で効率的である。

実装の詳細として、`.Length`プロパティは内部的には文字列オブジェクトのメタデータを参照し、そこに保存されている長さの値を返します。これは計算されるのではなく、文字列オブジェクトが作成される時点で設定されます。

## See Also
## 関連情報：
- Microsoft documentation on the String.Length Property: [https://docs.microsoft.com/en-us/dotnet/api/system.string.length](https://docs.microsoft.com/en-us/dotnet/api/system.string.length)
- An overview of strings in C#: [https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- Using LINQ in C#: [https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/)
