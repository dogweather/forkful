---
date: 2024-01-20 17:39:58.194789-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.022565-06:00'
model: gpt-4-1106-preview
summary: "C#\u3067\u306E\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u4F5C\u6210\u306F\u3001\
  \u7D44\u307F\u8FBC\u307F\u95A2\u6570`Path.GetTempFileName()`\u3067\u7C21\u5358\u306B\
  \u3067\u304D\u307E\u3059\u3002\u3053\u306E\u65B9\u6CD5\u306F\u3001\u30AA\u30DA\u30EC\
  \u30FC\u30C6\u30A3\u30F3\u30B0\u30B7\u30B9\u30C6\u30E0\u304C\u63D0\u4F9B\u3059\u308B\
  \u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306B\u5B89\
  \u5168\u306A\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u751F\u6210\u3057\u307E\u3059\
  \u3002\u904E\u53BB\u3067\u306F\u3001\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306F\u624B\
  \u52D5\u3067\u547D\u540D\u3084\u5834\u6240\u3092\u6C7A\u3081\u308B\u3053\u3068\u304C\
  \u4E00\u822C\u7684\u3067\u3057\u305F\u304C\u3001\u540D\u524D\u885D\u7A81\u306E\u30EA\
  \u30B9\u30AF\u3084\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u4E0A\u306E\u554F\u984C\u304C\
  \u3042\u3063\u305F\u305F\u3081\u3001\u81EA\u52D5\u751F\u6210\u304C\u6A19\u6E96\u3068\
  \u306A\u308A\u307E\u3057\u305F."
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 21
---

## How to (方法):
```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // 一時ファイルを作成
        string tempFilePath = Path.GetTempFileName();

        // 何かデータを書き込む
        File.WriteAllText(tempFilePath, "Hello, temporarily world!");

        // 一時ファイルからデータを読む
        string fileContent = File.ReadAllText(tempFilePath);
        Console.WriteLine(fileContent);  // 出力: Hello, temporarily world!

        // 一時ファイルを削除
        File.Delete(tempFilePath);
    }
}
```

## Deep Dive (掘り下げ):
C#での一時ファイル作成は、組み込み関数`Path.GetTempFileName()`で簡単にできます。この方法は、オペレーティングシステムが提供する一時ファイルディレクトリに安全な一時ファイルを生成します。過去では、一時ファイルは手動で命名や場所を決めることが一般的でしたが、名前衝突のリスクやセキュリティ上の問題があったため、自動生成が標準となりました。

代わりの方法として`Path.GetRandomFileName()`もありますが、これはファイルを物理的に作成せず、安全なランダムなファイル名だけを生成します。実装の詳細では、一時ファイルはシステム再起動時に消去されることが期待されるため、永続的なデータストレージには適していません。また、`System.IO`名前空間の他のクラスを使って、さらにリッチな操作が可能です。

## See Also (関連項目):
- [Microsoft: Path.GetTempFileName Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.path.gettempfilename)
- [Microsoft: File and Stream I/O](https://docs.microsoft.com/en-us/dotnet/standard/io/)
- [Stack Overflow: When should I use Path.GetRandomFileName() over Path.GetTempFileName()?](https://stackoverflow.com/questions/581570/how-can-i-create-a-temp-file-with-a-specific-extension-with-net)
