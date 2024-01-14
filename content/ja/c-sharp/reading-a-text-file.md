---
title:    "C#: 「テキストファイルを読み込む」"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ読み込みファイルをするのか？

プログラミングではテキストファイルを使用することがよくあります。テキストファイルにはさまざまな情報が保存されており、それを読み込むことでプログラムの柔軟性や機能性を向上させることができます。この記事では、C#で簡単にテキストファイルを読み込む方法をご紹介します。

## 方法：テキストファイルの読み込み

まず、ファイルを読み込むためには`StreamReader`クラスを使用します。以下のコードを使用して、指定したファイルを読み込みます。

```C#
StreamReader file = new StreamReader("ファイルパス");
```

次に、`ReadLine()`メソッドを使用して、ファイルから1行ずつデータを読み込みます。このメソッドを`while`ループで使用することで、ファイルの全てのデータを読み込むことができます。

```C#
while(file.Peek() >= 0) //ファイルからデータを読み込む
{
    string line = file.ReadLine(); //1行ずつデータを読み込む
    Console.WriteLine(line); //データを出力する
}
```

上記のコードを実行すると、ファイルから読み込んだデータが1行ずつコンソールに表示されます。

## ディープダイブ：テキストファイルの詳細

テキストファイルを読み込む際には、ファイルの文字エンコーディングに注意する必要があります。デフォルトでは、`StreamReader`クラスはUTF-8のエンコーディングを使用しますが、読み込むファイルによっては異なるエンコーディングを指定する必要があります。

また、ファイルを読み込む際にはエラーハンドリングも重要です。例えば、指定したファイルが存在しない場合やアクセス権限がない場合などには適切なエラーメッセージを表示することができるように、`try-catch`ブロックを使用して例外処理を実装することが推奨されています。

## 参考リンク

- [C#でテキストファイルを読み込む方法](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/file-system/how-to-read-from-a-text-file)
- [StreamReaderクラスのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.streamreader?view=netframework-4.8)
- [C#でエラーハンドリングを行う方法](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/exceptions/how-to-handle-a-generic-exception-in-csharp)