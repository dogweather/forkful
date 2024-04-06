---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:45.990986-07:00
description: "\u65B9\u6CD5\uFF1A C#\u306F`System.IO`\u30CD\u30FC\u30E0\u30B9\u30DA\
  \u30FC\u30B9\u3067\u30D5\u30A1\u30A4\u30EB\u64CD\u4F5C\u3092\u7C21\u7D20\u5316\u3057\
  \u3001\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u66F8\u304D\u8FBC\u3080\
  \u305F\u3081\u306E\u76F4\u63A5\u7684\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\
  \u3044\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\u57FA\u672C\u7684\u306A\u30C6\
  \u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u66F8\u304D\u8FBC\u3080\u65B9\u6CD5\
  \u3068\u3001\u65E2\u5B58\u306E\u30D5\u30A1\u30A4\u30EB\u306B\u30C6\u30AD\u30B9\u30C8\
  \u3092\u8FFD\u52A0\u3059\u308B\u65B9\u6CD5\u3092\u8AAC\u660E\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.148989-06:00'
model: gpt-4-0125-preview
summary: "C#\u306F`System.IO`\u30CD\u30FC\u30E0\u30B9\u30DA\u30FC\u30B9\u3067\u30D5\
  \u30A1\u30A4\u30EB\u64CD\u4F5C\u3092\u7C21\u7D20\u5316\u3057\u3001\u30C6\u30AD\u30B9\
  \u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u66F8\u304D\u8FBC\u3080\u305F\u3081\u306E\u76F4\
  \u63A5\u7684\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\
  \u3053\u3053\u3067\u306F\u3001\u57FA\u672C\u7684\u306A\u30C6\u30AD\u30B9\u30C8\u30D5\
  \u30A1\u30A4\u30EB\u3092\u66F8\u304D\u8FBC\u3080\u65B9\u6CD5\u3068\u3001\u65E2\u5B58\
  \u306E\u30D5\u30A1\u30A4\u30EB\u306B\u30C6\u30AD\u30B9\u30C8\u3092\u8FFD\u52A0\u3059\
  \u308B\u65B9\u6CD5\u3092\u8AAC\u660E\u3057\u307E\u3059."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

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
