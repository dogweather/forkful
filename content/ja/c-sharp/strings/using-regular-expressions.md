---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:57.038332-07:00
description: "C#\u2026"
lastmod: '2024-03-13T22:44:42.109080-06:00'
model: gpt-4-0125-preview
summary: "C# \u3067\u306E\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u306F\u6587\u5B57\
  \u5217\u5185\u306E\u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\u306B\u5BFE\
  \u3057\u3066\u5F37\u529B\u306A\u30C4\u30FC\u30EB\u3067\u3042\u308A\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u304C\u30C7\u30FC\u30BF\u3092\u691C\u7D22\u3001\u7F6E\u63DB\
  \u3001\u5206\u5272\u3001\u62BD\u51FA\u3059\u308B\u306E\u3092\u52B9\u7387\u7684\u306B\
  \u884C\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u305D\u306E\u67D4\u8EDF\u6027\u3068\u30D1\u30D5\u30A9\u30FC\u30DE\
  \u30F3\u30B9\u306E\u305F\u3081\u3001\u30E1\u30FC\u30EB\u30D5\u30A9\u30FC\u30DE\u30C3\
  \u30C8\u306E\u30C1\u30A7\u30C3\u30AF\u306E\u3088\u3046\u306A\u5358\u7D14\u306A\u691C\
  \u8A3C\u304B\u3089\u8907\u96D1\u306A\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u30BF\u30B9\
  \u30AF\u307E\u3067\u3001\u6B63\u898F\u8868\u73FE\u3092\u5229\u7528\u3057\u307E\u3059\
  \u3002."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 何となぜ？
C# での正規表現（regex）は文字列内のパターンマッチングに対して強力なツールであり、プログラマーがデータを検索、置換、分割、抽出するのを効率的に行うことができます。プログラマーはその柔軟性とパフォーマンスのため、メールフォーマットのチェックのような単純な検証から複雑なテキスト処理タスクまで、正規表現を利用します。

## どのように使うか：

### 単純なパターンマッチング
文字列が特定のパターンを含むかどうかをチェックするには、`System.Text.RegularExpressions` 名前空間から `Regex.IsMatch` メソッドを使用します。

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Hello, World!";
        string pattern = "World";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // 出力: True
    }
}
```

### データの抽出
regex のグループを使用して文字列からデータを抽出するには、`Regex.Match` メソッドを使用します。

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Date: 2023-04-12";
        string pattern = @"Date: (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(sampleText, pattern);

        if (match.Success)
        {
            Console.WriteLine($"Year: {match.Groups[1].Value}");  // 出力: Year: 2023
            Console.WriteLine($"Month: {match.Groups[2].Value}");  // 出力: Month: 04
            Console.WriteLine($"Day: {match.Groups[3].Value}");  // 出力: Day: 12
        }
    }
}
```

### テキストの置換
`Regex.Replace` メソッドを使うと、指定されたパターンに一致する文字列内のテキストを置換できます。

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Visit Microsoft!";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // 出力: Visit Google!
    }
}
```

### 文字列の分割
`Regex.Split` メソッドを使用して、regex パターンに基づいて文字列を配列に分割できます。

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "one,two,three,four,five";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // 出力: 
        // one
        // two
        // three
        // four
        // five
    }
}
```

### サードパーティのライブラリの使用
.NET Framework は正規表現のための広範なサポートを提供していますが、PCRE（Perl互換の正規表現）をC#で提供する `PCRE.NET` のようなサードパーティのライブラリも存在します。これは、.NET の実装では利用できない Perl の regex エンジンの機能や構文が必要な場合に役立ちます。

`PCRE.NET` を使用するには、まずその NuGet パッケージをインストールし、その後、ネイティブの .NET regex クラスを使用するのと同様に使用できます。

```csharp
// PCRE.NET を使用した例
// 注：PCRE.NET に特有の機能を紹介するためのサンプルと同様に想像してください。
```

正規表現のためのサードパーティのライブラリを統合する際は、詳細な使用方法や互換性情報について常にそのドキュメントを参照してください。
