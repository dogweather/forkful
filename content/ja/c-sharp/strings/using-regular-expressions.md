---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:57.038332-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u4F7F\u3046\u304B\uFF1A #."
lastmod: '2024-03-13T22:44:42.109080-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

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
