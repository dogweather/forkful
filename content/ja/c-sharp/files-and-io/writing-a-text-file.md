---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:45.990986-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.148989-06:00'
model: gpt-4-0125-preview
summary: "C#\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u66F8\u304D\
  \u8FBC\u3080\u3053\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u3067\u30C6\u30AD\
  \u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u30D5\u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\
  \u30E0\u4E0A\u306B\u4F5C\u6210\u307E\u305F\u306F\u5909\u66F4\u3059\u308B\u64CD\u4F5C\
  \u3067\u3042\u308A\u3001\u30ED\u30B0\u8A18\u9332\u3001\u30C7\u30FC\u30BF\u30A8\u30AF\
  \u30B9\u30DD\u30FC\u30C8\u3001\u307E\u305F\u306F\u8A2D\u5B9A\u7BA1\u7406\u306A\u3069\
  \u591A\u304F\u306E\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u3068\u3063\
  \u3066\u57FA\u672C\u7684\u306A\u30BF\u30B9\u30AF\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u3092\u30BB\u30C3\u30B7\u30E7\u30F3\
  \u9593\u3067\u6301\u7D9A\u3055\u305B\u305F\u308A\u3001\u30B7\u30B9\u30C6\u30E0\u9593\
  \u3067\u60C5\u5831\u3092\u5171\u6709\u3057\u305F\u308A\u3001\u4EBA\u9593\u304C\u8AAD\
  \u3081\u308B\u51FA\u529B\u3092\u683C\u7D0D\u3057\u305F\u308A\u3059\u308B\u305F\u3081\
  \u306B\u3053\u306E\u64CD\u4F5C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

## 何となぜ？
C#でテキストファイルを書き込むことは、プログラムでテキストファイルをファイルシステム上に作成または変更する操作であり、ログ記録、データエクスポート、または設定管理など多くのアプリケーションにとって基本的なタスクです。プログラマーは、データをセッション間で持続させたり、システム間で情報を共有したり、人間が読める出力を格納したりするためにこの操作を行います。

## 方法：
C#は`System.IO`ネームスペースでファイル操作を簡素化し、テキストファイルを書き込むための直接的な方法を提供しています。ここでは、基本的なテキストファイルを書き込む方法と、既存のファイルにテキストを追加する方法を説明します。

### スクラッチからテキストファイルに書き込む
```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "Hello, world!";

        // 新しいファイルに内容を書き込む
        File.WriteAllText(filePath, content);
        
        Console.WriteLine("ファイルの書き込みに成功しました。");
    }
}
```
**サンプル出力：**
```
ファイルの書き込みに成功しました。
```

### 既存のファイルにテキストを追加する
既存のファイルの末尾にテキストを追加したい場合は、`File.AppendAllText`メソッドを使用できます。

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string additionalContent = "\nさらにコンテンツを追加。";

        // ファイルにコンテンツを追加する
        File.AppendAllText(filePath, additionalContent);
        
        Console.WriteLine("コンテンツの追加に成功しました。");
    }
}
```
**サンプル出力：**
```
コンテンツの追加に成功しました。
```

### サードパーティライブラリの使用：`StreamWriter`
より細かい書き込み制御、自動フラッシュ、エンコーディング選択を含む、より複雑な書き込みシナリオには`StreamWriter`を使用します。

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "StreamWriterを使用しての例です。";

        // StreamWriterを使用してファイルに書き込む
        using (StreamWriter writer = new StreamWriter(filePath, append: true))
        {
            writer.WriteLine(content);
        }
        
        Console.WriteLine("FileStreamを使用したファイル書き込みに成功しました。");
    }
}
```
**サンプル出力：**
```
FileStreamを使用したファイル書き込みに成功しました。
```

これらのアプローチは、簡単な操作に直接`File`メソッドを使用し、より複雑な書き込みシナリオに`StreamWriter`を使用し、異なるニーズに対応します。パフォーマンスやファイルサイズなどの要因を考慮して、特定の要件に基づいて選択してください。
