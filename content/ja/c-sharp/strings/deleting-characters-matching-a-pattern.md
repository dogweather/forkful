---
date: 2024-01-20 17:41:48.160316-07:00
description: "\u6587\u5B57\u306B\u5408\u81F4\u3059\u308B\u30D1\u30BF\u30FC\u30F3\u3092\
  \u524A\u9664\u3059\u308B\u3068\u306F\u3001\u7279\u5B9A\u306E\u5F62\u5F0F\u3084\u6587\
  \u5B57\u3092\u542B\u3080\u30C6\u30AD\u30B9\u30C8\u304B\u3089\u305D\u308C\u3092\u53D6\
  \u308A\u9664\u304F\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u304C\u3053\u306E\u4F5C\u696D\u3092\u884C\u3046\u306E\u306F\u3001\u30C7\u30FC\u30BF\
  \u3092\u304D\u308C\u3044\u306B\u6574\u5F62\u3057\u305F\u308A\u3001\u4E0D\u8981\u306A\
  \u60C5\u5831\u3092\u6392\u9664\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u3067\u3059\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.252812
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u306B\u5408\u81F4\u3059\u308B\u30D1\u30BF\u30FC\u30F3\u3092\
  \u524A\u9664\u3059\u308B\u3068\u306F\u3001\u7279\u5B9A\u306E\u5F62\u5F0F\u3084\u6587\
  \u5B57\u3092\u542B\u3080\u30C6\u30AD\u30B9\u30C8\u304B\u3089\u305D\u308C\u3092\u53D6\
  \u308A\u9664\u304F\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u304C\u3053\u306E\u4F5C\u696D\u3092\u884C\u3046\u306E\u306F\u3001\u30C7\u30FC\u30BF\
  \u3092\u304D\u308C\u3044\u306B\u6574\u5F62\u3057\u305F\u308A\u3001\u4E0D\u8981\u306A\
  \u60C5\u5831\u3092\u6392\u9664\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u3067\u3059\
  \u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字に合致するパターンを削除するとは、特定の形式や文字を含むテキストからそれを取り除くことです。プログラマーがこの作業を行うのは、データをきれいに整形したり、不要な情報を排除したりするためです。

## How to: (方法)
```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string input = "Hello, World! 123";
        string pattern = @"\d";  // 数字にマッチするパターン
        
        // Regex.Replaceを使用して数字を削除
        string output = Regex.Replace(input, pattern, "");
        
        Console.WriteLine(output);  // "Hello, World! "
    }
}
```
このコードは`input`文字列から数字を取り除き、`output`に"Hello, World! "を表示します。

## Deep Dive (詳細な情報)
文字に合致するパターンを削除するニーズは、初期のコンピューティング時代にまでさかのぼります。様々な理由により、データは時として冗長な情報や、予期しない形式で存在します。C#では`System.Text.RegularExpressions`の`Regex`クラスを利用して簡単にパターンマッチと削除が行えます。

代替案として、`string.Replace`メソッドを使った単純な置換がありますが、これは正規表現のパターンには対応していません。また、LINQを使ったアプローチも可能ですが、正規表現の方が直感的でパワフルです。

パフォーマンスを考えると、`Regex`オブジェクトをコンパイルすることで処理速度を向上させることができます。これは複数回にわたって同じ正規表現を用いる場合に特に有効です。

## See Also (関連情報)
- .NETの正規表現ドキュメント: https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/regular-expression-language-quick-reference
- `Regex.Replace`メソッド: https://docs.microsoft.com/ja-jp/dotnet/api/system.text.regularexpressions.regex.replace
- C#での文字列操作についての一般的なガイド: https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/strings/
