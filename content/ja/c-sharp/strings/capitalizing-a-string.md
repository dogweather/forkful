---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:25.678937-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A\u2026"
lastmod: '2024-03-13T22:44:42.100303-06:00'
model: gpt-4-0125-preview
summary: "C#\u306F\u3001\u7D44\u307F\u8FBC\u307F\u306E\u30E1\u30BD\u30C3\u30C9\u3092\
  \u4F7F\u7528\u3057\u3066\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\
  \u76F4\u63A5\u7684\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u3053\
  \u308C\u3092\u9054\u6210\u3059\u308B\u6700\u3082\u7C21\u5358\u306A\u65B9\u6CD5\u306F\
  \u3001\u3053\u308C\u3089\u306E\u30E1\u30BD\u30C3\u30C9\u3067\u6587\u5B57\u5217\u3092\
  \u76F4\u63A5\u4FEE\u6B63\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u3088\u308A\u8907\
  \u96D1\u307E\u305F\u306F\u7279\u5B9A\u306E\u5927\u6587\u5B57\u5316\u30EB\u30FC\u30EB\
  \uFF08\u4F8B\u3048\u3070\u3001\u5404\u5358\u8A9E\u306E\u6700\u521D\u306E\u6587\u5B57\
  \u3092\u5927\u6587\u5B57\u306B\u3059\u308B\uFF09\u306B\u3064\u3044\u3066\u306F\u3001\
  \u8FFD\u52A0\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3084\u624B\u52D5\u306E\u65B9\u6CD5\
  \u304C\u5FC5\u8981\u306B\u306A\u308B\u304B\u3082\u3057\u308C\u307E\u305B\u3093\u3002\
  \u4EE5\u4E0B\u306B\u3001C#\u3067\u69D8\u3005\u306A\u65B9\u6CD5\u3067\u6587\u5B57\
  \u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3059\u4F8B\
  \u3092\u7D39\u4ECB\u3057\u307E\u3059."
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## どのように：
C#は、組み込みのメソッドを使用して文字列を大文字化する直接的な方法を提供します。これを達成する最も簡単な方法は、これらのメソッドで文字列を直接修正することです。より複雑または特定の大文字化ルール（例えば、各単語の最初の文字を大文字にする）については、追加のライブラリや手動の方法が必要になるかもしれません。以下に、C#で様々な方法で文字列を大文字化する方法を示す例を紹介します。

### 基本的な大文字化：
単語または文の最初の文字を大文字にするには：

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // 出力: "Hello world"
```

### 各単語の最初の文字を大文字にする：
文字列の各単語の最初の文字を大文字にするためには、`System.Globalization`名前空間にある`TextInfo.ToTitleCase`メソッドを使用できます：

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // 出力: "Hello World"
```

注意：`ToTitleCase`は残りの文字のケースを小文字には変えず、単語の最初の文字のみを大文字に変更します。また、タイトルケースルールの特定の単語（「and」、「or」、「of」など）は、文化設定によって大文字にされない場合があります。

### 再利用性のための拡張メソッドの使用：
大文字化プロセスを簡素化するために`string`クラスの拡張メソッドを作成し、コードをよりクリーンで再利用可能にすることができます。以下は、そのようなメソッドを作成して使用する方法です：

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // 出力: "Hello world"
    }
}
```

この拡張メソッド`Capitalize`は、名前空間内の任意の文字列オブジェクトで呼び出すことができ、C#での文字列操作をより直感的でオブジェクト指向的なアプローチを提供します。

### サードパーティライブラリの使用:
C#の標準ライブラリは、文字列の大文字化や、文字列の各単語を大文字化するためのほとんどのニーズをカバーしている一方で、特定の特化したタスクには、Humanizerのようなサードパーティライブラリが有益な場合があります。しかし、単に文字列や各単語を大文字化するタスクのためには、標準のC#メソッドが十分で効率的であり、外部依存性の必要性を否定します。
