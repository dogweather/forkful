---
title:                "テキストファイルの読み込み"
html_title:           "C#: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

あなたがこのテキストファイルを読み込むことに興味があるかもしれませんが、それがなぜ重要なのでしょうか？テキストファイルを読み込むことは、データを取得し、処理するために非常に役立ちます。たとえば、データベースにデータを保存する前に、テキストファイルに書き出してバックアップを作成することができます。また、コンピューターやネットワーク上でデータを共有する際にも、テキストファイルの読み込みが必要になる場合があります。この記事では、C#を使用してテキストファイルを読み込む方法について説明します。

## 方法

まずはテキストファイルを読み込むには、ファイルを開く必要があります。C#では、`File.OpenText()`メソッドを使用することでファイルを開くことができます。ファイルを開いたら、`StreamReader`オブジェクトを使用してファイルの内容を読み込むことができます。以下のコード例を参考にしてください。

```C#
// ファイルを開く
StreamReader file = File.OpenText("テキストファイルのパス");

// ファイルの内容を1行ずつ読み込む
string line = "";
while((line = file.ReadLine()) != null)
{
    Console.WriteLine(line);
}

// ファイルを閉じる
file.Close();
```

上記のコードでは、`File.OpenText()`メソッドを使用してファイルを開き、`StreamReader`オブジェクトを使用して1行ずつファイルの内容を読み込んでいます。そして最後に、ファイルを閉じることでリソースの解放を行なっています。この方法を使用することで、テキストファイルの内容を簡単に読み込むことができます。

また、もしファイルの内容を全て読み込んでから処理するのではなく、特定の条件に合致した行のみを処理したい場合は、`StreamReader`オブジェクトの`ReadLine()`メソッドではなく、`ReadLineAsync()`メソッドを使用することで非同期に処理を行うこともできます。

## 深堀り

テキストファイルを読み込む際に、文字コードの設定が重要になります。デフォルトでは、`StreamReader`オブジェクトはテキストファイルをUTF-8の文字コードで読み込みますが、もし異なる文字コードで作成されたファイルを読み込む場合は、`Encoding`クラスを使用して明示的に指定することが必要です。また、`Encoding`クラスを使用して、テキストファイルの文字コードを変換することも可能です。詳しくは公式ドキュメントを参照してください。

## 参考リンク

- [File.OpenText() メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.file.opentext)
- [StreamReader クラス](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.streamreader)
- [Encoding クラス](https://docs.microsoft.com/ja-jp/dotnet/api/system.text.encoding)
- [非同期にテキストファイルを読み込む方法](https://www.inf