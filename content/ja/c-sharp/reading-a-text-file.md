---
title:    "C#: 「テキストファイルの読み込み」"
keywords: ["C#"]
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読み込むことは、プログラマーにとって非常に重要なスキルです。データ処理やファイル操作を行うために、テキストファイルを読み込む必要があります。この記事では、C#でテキストファイルを読み込む方法について説明します。

## 方法

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // テキストファイルを読み込むためのStreamReaderオブジェクトを作成
        StreamReader reader = new StreamReader("sample.txt");

        // ファイルの最後まで一行ずつ読み込む
        string line;
        while ((line = reader.ReadLine()) != null)
        {
            // 読み込んだ内容を出力する
            Console.WriteLine(line);
        }

        // ファイルを閉じる
        reader.Close();
    }
}
```

上記のコードは、"sample.txt"という名前のテキストファイルを読み込み、ファイルの内容を一行ずつ出力します。

下記は、テキストファイルの内容が以下のように書かれている場合の例です。

```
Hello
こんにちは
Hola
```

上記のコードを実行すると、以下のように出力されます。

```
Hello
こんにちは
Hola
```

このように、テキストファイルの内容を読み込むことができます。

## 深堀り

テキストファイルを読み込む際には、ファイルのエンコーディングや改行コードに注意する必要があります。例えば、日本語のテキストファイルを読み込む場合は、適切なエンコーディングを指定する必要があります。

また、より高度な操作では、ファイルの内容をパースして特定のデータを抽出することもできます。例えば、CSV形式のファイルを読み込んでデータベースに格納することなどが考えられます。

テキストファイルの読み込みについてさらに詳しく学びたい方は、C#のドキュメントやオンラインリソースを参考にしてみてください。

## 参考リンク

- [Microsoft Docs: ファイルおよびストリームの入出力](https://docs.microsoft.com/ja-jp/dotnet/standard/io/)
- [C# ステータスマシンの読み込み方法](https://docs.microsoft.com/ja-jp/dotnet/csharp/language-reference/keywords/iterator)
- [テキストファイルを扱うための.NET APIの概要](https://docs.microsoft.com/ja-jp/dotnet/standard/io/how-to-read-text-from-a-file)
- [C# プログラマー向け読書リスト](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/concepts/)
- [エンコーディングを使用してファイルからテキストを読み取る](https://docs.microsoft.com/ja-jp/dotnet/standard/io/how-to-read-text-from-a-file)