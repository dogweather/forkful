---
date: 2024-01-20 17:41:48.160316-07:00
description: "How to: (\u65B9\u6CD5) \u3053\u306E\u30B3\u30FC\u30C9\u306F`input`\u6587\
  \u5B57\u5217\u304B\u3089\u6570\u5B57\u3092\u53D6\u308A\u9664\u304D\u3001`output`\u306B\
  \"Hello, World! \"\u3092\u8868\u793A\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:41.645324-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u3053\u306E\u30B3\u30FC\u30C9\u306F`input`\u6587\u5B57\u5217\
  \u304B\u3089\u6570\u5B57\u3092\u53D6\u308A\u9664\u304D\u3001`output`\u306B\"Hello,\
  \ World!."
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
weight: 5
---

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
