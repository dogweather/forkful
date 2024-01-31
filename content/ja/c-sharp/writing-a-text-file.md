---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ?)
テキストファイルの書き込みは、データを永続的に保存するプロセスです。プログラマは設定、ログ、保存したい情報を外部に記録するために使います。

## How to: (やり方)
```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = "sample.txt";
        string textToAdd = "こんにちは、ファイル!";

        File.WriteAllText(filePath, textToAdd);
        
        // ファイル内容を読み込み
        string readText = File.ReadAllText(filePath);
        Console.WriteLine(readText); // 出力: こんにちは、ファイル!
    }
}
```

## Deep Dive (詳細情報)
ファイルを書き込む機能は、初期のコンピュータシステムから存在します。`File.WriteAllText`などのメソッドは.NETでの作業を簡単にし、バイトまたはテキストデータの操作を直感的に行えます。ストリームを使う方法（`StreamWriter`など）もあり、大きなデータや連続的な書き込みに向いています。

## See Also (関連情報)
- Microsoft Docs on File.WriteAllText: [https://docs.microsoft.com/en-us/dotnet/api/system.io.file.writealltext](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.writealltext)
