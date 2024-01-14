---
title:                "C#: テキストファイルの作成"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを書くことの利点は何でしょうか？プログラミングを学ぶ上で、テキストファイルを書くことは非常に重要です。その理由を見てみましょう。

## 書き方

C#を使用してテキストファイルを書く方法を紹介します。以下はコードブロック内での例と出力です。

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // テキストファイルを作成する
        using (StreamWriter writer = File.CreateText("sampletxt.txt"))
        {
            writer.WriteLine("こんにちは、世界！");
            writer.WriteLine("これはテキストファイルです。");
        }

        // テキストファイルを読み込む
        StreamReader reader = new StreamReader("sampletxt.txt");
        while (reader.Peek() >= 0)
        {
            Console.WriteLine(reader.ReadLine());
        }
        reader.Close();
    }
}
```

出力:

```
こんにちは、世界！
これはテキストファイルです。
```

## 深堀り

テキストファイルを書くことについてもっと詳しく知りたいですか？テキストファイルは、文字列やデータを格納するための簡単で使いやすい方法です。プログラムからテキストファイルを読み書きすることで、ファイルを保存したり読み込んだりすることができます。さらに、テキストファイルを使用すると、複数のプログラム間でデータを共有することができます。

## 参考リンク

- [C# ファイルを使用したテキストデータの読み書き](https://howtoprogramwithjupyter.c