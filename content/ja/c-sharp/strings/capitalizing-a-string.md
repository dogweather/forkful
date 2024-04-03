---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:25.678937-07:00
description: "C#\u3067\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\u5B57\u3092\
  \u5927\u6587\u5B57\u306B\u3059\u308B\u3053\u3068\u306F\u3001\u305D\u306E\u6587\u5B57\
  \u304C\u65E2\u306B\u5927\u6587\u5B57\u3067\u306A\u3044\u5834\u5408\u306B\u3001\u6700\
  \u521D\u306E\u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\u3053\
  \u3068\u3092\u542B\u307F\u307E\u3059\u3002\u3053\u306E\u5909\u66F4\u306F\u3001\u51FA\
  \u529B\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3001\u30B3\u30FC\u30C7\u30A3\u30F3\
  \u30B0\u6A19\u6E96\u306E\u9069\u7528\u3001\u307E\u305F\u306F\u30E6\u30FC\u30B6\u30FC\
  \u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u30C6\u30AD\u30B9\u30C8\u306E\u8AAD\
  \u307F\u3084\u3059\u3055\u3092\u5411\u4E0A\u3055\u305B\u308B\u305F\u3081\u306B\u3001\
  \u91CD\u8981\u306A\u5834\u5408\u304C\u3042\u308A\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.100303-06:00'
model: gpt-4-0125-preview
summary: "C#\u3067\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\
  \u6587\u5B57\u306B\u3059\u308B\u3053\u3068\u306F\u3001\u305D\u306E\u6587\u5B57\u304C\
  \u65E2\u306B\u5927\u6587\u5B57\u3067\u306A\u3044\u5834\u5408\u306B\u3001\u6700\u521D\
  \u306E\u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\
  \u3092\u542B\u307F\u307E\u3059\u3002\u3053\u306E\u5909\u66F4\u306F\u3001\u51FA\u529B\
  \u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3001\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\
  \u6A19\u6E96\u306E\u9069\u7528\u3001\u307E\u305F\u306F\u30E6\u30FC\u30B6\u30FC\u30A4\
  \u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u30C6\u30AD\u30B9\u30C8\u306E\u8AAD\u307F\
  \u3084\u3059\u3055\u3092\u5411\u4E0A\u3055\u305B\u308B\u305F\u3081\u306B\u3001\u91CD\
  \u8981\u306A\u5834\u5408\u304C\u3042\u308A\u307E\u3059\u3002."
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
