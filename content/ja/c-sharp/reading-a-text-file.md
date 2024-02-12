---
title:                "テキストファイルの読み込み"
aliases:
- ja/c-sharp/reading-a-text-file.md
date:                  2024-01-20T17:53:52.721030-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストファイルの読み込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/reading-a-text-file.md"
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
