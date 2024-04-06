---
date: 2024-01-20 17:53:52.721030-07:00
description: "How to: (\u65B9\u6CD5) C#\u3067\u306E\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\
  \u30A4\u30EB\u306E\u8AAD\u307F\u53D6\u308A\u306F\u7C21\u5358\u3067\u3059\u3002\u4EE5\
  \u4E0B\u306F\u57FA\u672C\u7684\u306A\u4F8B."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.019910-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) C#\u3067\u306E\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\
  \u306E\u8AAD\u307F\u53D6\u308A\u306F\u7C21\u5358\u3067\u3059\u3002\u4EE5\u4E0B\u306F\
  \u57FA\u672C\u7684\u306A\u4F8B."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

## How to: (方法)
C#でのテキストファイルの読み取りは簡単です。以下は基本的な例:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\path\to\your\file.txt";
        if (File.Exists(filePath))
        {
            string content = File.ReadAllText(filePath);
            Console.WriteLine(content);
        }
        else
        {
            Console.WriteLine("ファイルが見つかりません。");
        }
    }
}
```

サンプル出力:
```
こんにちは、これはテキストファイルです。
次の行にようこそ。
```

## Deep Dive (詳細解説)
C#が.NET Frameworkで初めて導入された時から、テキストファイルの読み取り機能は基本的な入出力の一部でした。より高度な利用では、`StreamReader`や`FileStream`を使って内容を少しずつ読み込むことができます。これによりメモリの使用が抑えられます。`async`と`await`キーワードを使って非同期的にファイルを読むことも可能です。これはアプリケーションのレスポンス性を向上させます。さらに、ライブラリが提供するさまざまなエンコーディングオプションを使って、さまざまなテキスト形式を読み込むことができます。

## See Also (関連情報)
- [StreamReaderとFileStreamの使用](https://docs.microsoft.com/ja-jp/dotnet/standard/io/how-to-read-text-from-a-file)
- [非同期ファイル I/O](https://docs.microsoft.com/ja-jp/dotnet/standard/io/asynchronous-file-i-o)
- [ファイルとストリーム入出力](https://docs.microsoft.com/ja-jp/dotnet/standard/io/)
- [ファイルのエンコーディング](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/character-encoding)
