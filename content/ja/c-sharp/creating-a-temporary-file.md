---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ？

一時ファイルの作成は、一時的なデータ保管場所を提供するプロセスです。プログラマーは、大きなデータ操作を行う際や、ファイルのパフォーマンスを改善したいときにこれを行います。

## 使い方：

C#では、PathクラスのGetTempFileNameメソッドを使用して一時ファイルを作成できます。

```C#
using System.IO;

class Program {
    static void Main() {
        string tempFile = Path.GetTempFileName();

        Console.WriteLine("Temp File: " + tempFile);
    }
}
```

上記のコードを実行すると、次のような出力が得られます。

```markdown
Temp File: C:\Users\Username\AppData\Local\Temp\tmpXXXX.tmp
```
ここで、tmpXXXXは一時的ファイル名です。

## ディープダイブ：

一時ファイルの作成は、多くの複雑なソフトウェアシステムの設計に不可欠な要素です。これらのファイルはシステムが大量のデータを処理するために使用され、ソフトウェアのパフォーマンスと安定性を向上させます。

一時ファイルを作成するための他の方法としては、FileクラスのCreateメソッドを使用し、想定されるファイルパスを指定することが挙げられます。しかし、この方法は一時ファイル名の一意性を保証しないため、GetTempFileNameメソッドの使用が一般的です。

## 参考資料：

1. [Path.GetTempFileName メソッド (System.IO)](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.path.gettempfilename?view=net-5.0)
2. [File.Create メソッド (System.IO)](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.file.create?view=net-5.0)