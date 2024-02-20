---
date: 2024-01-20 17:53:52.721030-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\
  \u307F\u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u30D7\u30ED\
  \u30B0\u30E9\u30E0\u306B\u53D6\u308A\u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\u30C7\
  \u30FC\u30BF\u306E\u53D6\u5F97\u3001\u8A2D\u5B9A\u306E\u8AAD\u307F\u8FBC\u307F\u3001\
  \u307E\u305F\u306F\u30ED\u30B0\u60C5\u5831\u306E\u5206\u6790\u306A\u3069\u3001\u5B9F\
  \u7528\u6027\u8C4A\u304B\u306A\u7406\u7531\u3067\u958B\u767A\u8005\u304C\u884C\u3044\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.294809
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\
  \u307F\u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u30D7\u30ED\
  \u30B0\u30E9\u30E0\u306B\u53D6\u308A\u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\u30C7\
  \u30FC\u30BF\u306E\u53D6\u5F97\u3001\u8A2D\u5B9A\u306E\u8AAD\u307F\u8FBC\u307F\u3001\
  \u307E\u305F\u306F\u30ED\u30B0\u60C5\u5831\u306E\u5206\u6790\u306A\u3069\u3001\u5B9F\
  \u7528\u6027\u8C4A\u304B\u306A\u7406\u7531\u3067\u958B\u767A\u8005\u304C\u884C\u3044\
  \u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキストファイルの読み込みとは、ファイルの内容をプログラムに取り込むことです。データの取得、設定の読み込み、またはログ情報の分析など、実用性豊かな理由で開発者が行います。

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
