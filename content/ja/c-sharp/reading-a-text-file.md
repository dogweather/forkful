---
title:                "C#: テキストファイルの読み込み"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

こんにちは、プログラマーの皆さん！日本語でのプログラミングブログの紹介です。今回は、C#でテキストファイルを読み取る方法についてお話しします。

## なぜ読み取るのか？

テキストファイルを読み取ることは、プログラミングにおいて非常に重要です。例えば、データベースからデータを取得して処理するのではなく、テキストファイルからデータを読み取って処理することもできます。また、プログラムの設定や設定ファイルにもよく使用されます。

## 読み取りの方法

まず始めに、テキストファイルを読み取るためには、`StreamReader`というメソッドを使用します。下記のコード例をご覧ください。

```C#
// ファイルのパスを指定
string filePath = "C:\\Users\\User\\Desktop\\sample.txt";

// StreamReaderオブジェクトを作成する
StreamReader reader = new StreamReader(filePath);

// ファイルから1行ずつ読み取り、出力する
string line = "";
while ((line = reader.ReadLine()) != null)
{
    Console.WriteLine(line);
}

// ファイルを閉じる
reader.Close();
```

上記のコードを実行すると、指定したテキストファイルの内容が1行ずつ出力されます。例えば、下記のようなテキストファイルを読み込んだ場合、

```
こんにちは
こんばんは
おはよう
```

次のように出力されます。

```
こんにちは
こんばんは
おはよう
```

## 詳しく見ていく

テキストファイルを読み取る際に、より詳細な情報が必要になることもあるかもしれません。その場合は、`StreamReader`クラスの他のメソッドやプロパティを使用することができます。例えば、`Peek()`メソッドを使用すると、今読み取っている文字の次の文字を取得することができます。また、`Current`プロパティを使用すると、現在の文字を取得することができます。詳しくはC#のドキュメントをご覧ください。

それでは、今回のブログではここまでとしましょう。テキストファイルを読み取る方法についてご紹介しました。この方法を使えば、データの処理にテキストファイルを活用することができます。是非、実際にお試しください！

## 以下も参考にしてみてください

- [Microsoft公式ドキュメント：StreamReaderクラス](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.streamreader?view=net-5.0)
- [C#でテキストファイルを書き込む方法](https://www.geeksforgeeks.org/how-to-write-a-text-file-in-c-sharp/)
- [C#でテキストファイルを読み込む方法(動画)](https://www.youtube.com/watch?v=2wms4JziAI0)

お疲れ様でした！