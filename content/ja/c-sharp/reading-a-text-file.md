---
title:                "テキストファイルの読み込み"
html_title:           "Bash: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何となぜ？

テキストファイルを読むとは、あるプログラムがTEXT形式で保存されたデータを理解可能な形に変換することです。これをプログラマは、データを解析し、操作し、最終的には情報を抽出するために行います。

## 方法:

以下はC#でテキストファイルを読み込む基本的な方法の一つです:

```C#
using System.IO;

class Program
{
    static void Main()
    {
    using StreamReader reader = new StreamReader("file.txt");
    string line;
    while((line = reader.ReadLine()) != null)
    {
        System.Console.WriteLine(line);
    }
    }
}
```

これが出力されます:

```C#
First line of file.
Second line of file.
...
```

## 深掘り：

テキストファイルの読み込みは、プログラミングの歴史において早い段階から行われていました。これは、情報を共有し、保存し、後で取り出す最もシンプルな方法でした。

他の方法としては、`File.ReadAllText()`,`File.ReadAllLines()`といった方法もあります。しかし、これらは全ての行を一度に読むため、大きなファイルの場合にはメモリの問題が発生する可能性があります。

詳細を見ると、`StreamReader`は内部でバッファリングを行い、その結果としてパフォーマンスが向上します。また、ディスクからの読み取りを最小限に抑えます。

## 参考になるもの:

- [Microsoft: StreamReader Class](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.streamreader?view=net-5.0)
- [Microsoft: File and Stream I/O](https://docs.microsoft.com/ja-jp/dotnet/standard/io/)