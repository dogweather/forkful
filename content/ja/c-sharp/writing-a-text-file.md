---
title:                "C#: テキストファイルの書き方"
simple_title:         "テキストファイルの書き方"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ
テキストファイルを作成することのメリットはたくさんあります。たとえば、プログラムで使用するデータを整理したり、テキスト文書を作成したりすることができます。また、過去のデータを保管するためにもテキストファイルを使用することができます。

## 作り方
まずはテキストファイルを作成するために必要なC#コードを書きましょう。以下は基本的な例です。

```C#
using System;
using System.IO;

// ファイルを作成し、テキストを書き込む
using (StreamWriter writer = new StreamWriter("example.txt")) 
{
    writer.WriteLine("こんにちは、世界！");
}

// ファイルからテキストを読み込む
using (StreamReader reader = new StreamReader("example.txt")) 
{
    string line;
    while ((line = reader.ReadLine()) != null) 
    {
        Console.WriteLine(line);
    }
}
```

実行すると、"example.txt"ファイルに"こんにちは、世界！"という文字列が書き込まれ、プログラムから読み取ったテキストがコンソールに出力されます。

## 深堀り
テキストファイルを作成する際には、さまざまなオプションも利用することができます。たとえば、ファイルの拡張子や文字エンコーディングを指定することができます。また、ファイルに追記する際には`StreamWriter`コンストラクタの第二引数に`true`を渡すことで、既存のファイルに追記することができます。

さらに詳しい情報を学びたい方は、マイクロソフトの公式ドキュメントやオンラインのチュートリアルを参考にしてください。

## 関連情報
- [C#によるテキストファイルの作成](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/file-system/how-to-write-to-a-text-file)
- [C#でファイルの読み書きをする方法](https://www.javadrive.jp/csharp/file/index1.html)
- [C# ファイル操作の基礎](https://www.sejuku.net/blog/6109)