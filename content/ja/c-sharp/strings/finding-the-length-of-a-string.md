---
date: 2024-01-20 17:47:01.839401-07:00
description: "\u4F55\u3068\u306A\u305C\uFF1F \u6587\u5B57\u5217\u306E\u9577\u3055\u3092\
  \u898B\u3064\u3051\u308B\u3068\u3044\u3046\u306E\u306F\u3001\u5358\u7D14\u306B\u6587\
  \u5B57\u5217\u304C\u4F55\u6587\u5B57\u304B\u3089\u6210\u3063\u3066\u3044\u308B\u304B\
  \u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u306E\u64CD\u4F5C\u306F\
  \u3001\u5165\u529B\u306E\u691C\u8A3C\u3001\u30C7\u30FC\u30BF\u306E\u6574\u5F62\u3001\
  \u30EB\u30FC\u30D7\u3067\u306E\u51E6\u7406\u306A\u3069\u3001\u3055\u307E\u3056\u307E\
  \u306A\u72B6\u6CC1\u3067\u5F79\u7ACB\u3061\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.109795-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u3068\
  \u3044\u3046\u306E\u306F\u3001\u5358\u7D14\u306B\u6587\u5B57\u5217\u304C\u4F55\u6587\
  \u5B57\u304B\u3089\u6210\u3063\u3066\u3044\u308B\u304B\u3092\u6570\u3048\u308B\u3053\
  \u3068\u3067\u3059\u3002\u3053\u306E\u64CD\u4F5C\u306F\u3001\u5165\u529B\u306E\u691C\
  \u8A3C\u3001\u30C7\u30FC\u30BF\u306E\u6574\u5F62\u3001\u30EB\u30FC\u30D7\u3067\u306E\
  \u51E6\u7406\u306A\u3069\u3001\u3055\u307E\u3056\u307E\u306A\u72B6\u6CC1\u3067\u5F79\
  \u7ACB\u3061\u307E\u3059."
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

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
