---
title:                "ディレクトリが存在するかどうかを確認する"
date:                  2024-01-19
simple_title:         "ディレクトリが存在するかどうかを確認する"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コードでディレクトリが存在するかを確認するのは基本操作です。これは、ファイル操作や設定の保存などの前に、エラーを防ぐために行います。

## How to: (やり方)
C#では、`System.IO` 名前空間の `Directory` クラスの `Exists` メソッドを使ってディレクトリが存在するかをチェックします。

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string path = @"C:\MyFolder";

        if (Directory.Exists(path))
        {
            Console.WriteLine("ディレクトリは存在します。");
        }
        else
        {
            Console.WriteLine("ディレクトリは存在しません。");
        }
    }
}
```

このコードは、`C:\MyFolder` ディレクトリがあるかどうかをチェックし、その結果を出力します。

## Deep Dive (掘り下げ)
最初は、存在チェックは単にファイルのリストを得て確認するだけでした。しかし、現在では`Directory.Exists` メソッドは効率的で信頼性があります。このメソッド以外に、`FileInfo` や `DirectoryInfo` オブジェクトを使っても同様のチェックができますが、`Directory.Exists` がもっとも直接的で簡単です。

## See Also (関連項目)
- .NET API Documentation for `Directory.Exists`: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- サンプルコードと解説が豊富な `.NET` 開発者向けコミュニティ: [Stack Overflow](https://stackoverflow.com/questions/tagged/c%23)
