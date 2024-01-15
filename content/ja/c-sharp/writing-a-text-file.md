---
title:                "テキストファイルの作成"
html_title:           "C#: テキストファイルの作成"
simple_title:         "テキストファイルの作成"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを作成するために取り組む理由は何ですか？それは、テキストファイルを使用してデータを保存して保管することができるからです。たとえば、コンピュータープログラムを実行するときに、そのプログラムが必要とするデータをテキストファイルに保存しておくことで、プログラムが必要なデータを簡単に取得することができます。

## 作り方

```C#
using System;
using System.IO;

class Program 
{
    static void Main(string[] args)
    {
        // テキストファイルの作成と書き込み
        File.WriteAllText("example.txt", "これはテストです。");

        // テキストファイルからデータを読み込む
        string data = File.ReadAllText("example.txt");
        Console.WriteLine(data);
    }
}
```

上の例では、C#の`File`クラスを使用してテキストファイルを作成し、書き込み、読み込みする方法を示しています。`WriteAllText`メソッドを使用すると、指定したファイル名とデータを渡すことで、テキストファイルを作成し、内容を書き込むことができます。同様に、`ReadAllText`メソッドを使用すると、指定したファイル名からテキストファイルの内容を読み込むことができます。

## 詳細を掘り下げる

テキストファイルを作成する方法は他にもあります。例えば、`StreamWriter`クラスを使用することで、ファイルを開いてデータを書き込むことができます。また、`File.ReadAllText`メソッドでは、テキストファイル以外のファイルを読み込むこともできます。さらに、ファイルの存在をチェックするための`File.Exists`メソッドや、ファイルの削除を行う`File.Delete`メソッドなど、さまざまなメソッドが用意されています。

## 関連リンク

- [C#ドキュメント](https://docs.microsoft.com/ja-jp/dotnet/csharp/)
- [File.WriteAllTextメソッドのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.file.writealltext?view=netcore-3.1)
- [StreamWriterクラスのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.streamwriter?view=netcore-3.1)
- [File.Existsメソッドのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.file.exists?view=netcore-3.1)
- [File.Deleteメソッドのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.file.delete?view=netcore-3.1)