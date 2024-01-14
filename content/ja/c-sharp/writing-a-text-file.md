---
title:    "C#: テキストファイルの書き方"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを書くことには、コンピュータの命令を格納するための手段があります。これにより、プログラマーはファイルから情報を読み取ったり、編集したりすることができます。

## やり方

テキストファイルを書くには、まずファイルを作成する必要があります。次に、ファイルに書き込むためのコードを書きます。最後に、ファイルを閉じます。ここでは、C#言語を使用して、テキストファイルを書く方法を紹介します。

```C#
using System;
using System.IO;

// ファイルを作成する
StreamWriter file = new StreamWriter("example.txt");

// ファイルに文字列を書き込む
file.WriteLine("こんにちは、世界！");

// ファイルを閉じる
file.Close();

// ファイルを読み取る
StreamReader readFile = new StreamReader("example.txt");

// ファイルの内容を出力する
Console.WriteLine(readFile.ReadToEnd());

// ファイルを閉じる
readFile.Close();
```

ここでは、`StreamWriter`クラスを使用してファイルを作成し、`WriteLine`メソッドを使用してテキストを書き込みます。また、`StreamReader`クラスを使用してファイルを読み取り、`ReadToEnd`メソッドを使用してファイルの内容を出力します。最後に、ファイルを閉じることを忘れないようにしましょう。

## 深堀り

テキストファイルを書く際には、様々なオプションがあります。例えば、ファイルのパスやエンコーディングを指定することもできます。また、C#以外の言語でも同様の方法でテキストファイルを書くことができます。

## 参考リンク

- [C# StreamWriter Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter?view=net-5.0)
- [C# StreamReader Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=net-5.0)
- [C# Writing to Text Files Tutorial](https://www.tutorialspoint.com/csharp/csharp_writing_to_text_files.htm)

## 関連記事を見る