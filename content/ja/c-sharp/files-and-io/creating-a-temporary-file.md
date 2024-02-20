---
date: 2024-01-20 17:39:58.194789-07:00
description: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306F\u3001\u30C7\u30FC\u30BF\u3092\
  \u4E00\u6642\u7684\u306B\u4FDD\u7BA1\u3059\u308B\u305F\u3081\u306E\u30D5\u30A1\u30A4\
  \u30EB\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\
  \u51E6\u7406\u4E2D\u306B\u4E2D\u9593\u7D50\u679C\u3092\u4FDD\u5B58\u3059\u308B\u305F\
  \u3081\u3084\u3001\u5927\u91CF\u306E\u30C7\u30FC\u30BF\u3092\u6271\u3046\u969B\u306B\
  \u30EA\u30BD\u30FC\u30B9\u3092\u7BC0\u7D04\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u4F5C\u6210\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.296887
model: gpt-4-1106-preview
summary: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306F\u3001\u30C7\u30FC\u30BF\u3092\
  \u4E00\u6642\u7684\u306B\u4FDD\u7BA1\u3059\u308B\u305F\u3081\u306E\u30D5\u30A1\u30A4\
  \u30EB\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\
  \u51E6\u7406\u4E2D\u306B\u4E2D\u9593\u7D50\u679C\u3092\u4FDD\u5B58\u3059\u308B\u305F\
  \u3081\u3084\u3001\u5927\u91CF\u306E\u30C7\u30FC\u30BF\u3092\u6271\u3046\u969B\u306B\
  \u30EA\u30BD\u30FC\u30B9\u3092\u7BC0\u7D04\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u4F5C\u6210\u3057\u307E\u3059\u3002"
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
一時ファイルは、データを一時的に保管するためのファイルです。プログラマーはデータ処理中に中間結果を保存するためや、大量のデータを扱う際にリソースを節約するためにこれを作成します。

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
