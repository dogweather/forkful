---
title:                "「テキストファイルの読み込み」"
html_title:           "Gleam: 「テキストファイルの読み込み」"
simple_title:         "「テキストファイルの読み込み」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ？

テキストファイルを読むことには、さまざまな理由があります。たとえば、ソフトウェア開発者はテキストファイルからデータを読み取り、プログラムにそのデータを使用することができます。また、作文家は自分の作品をテキストファイルに保存しておき、後で編集することができます。この記事では、テキストファイルを読む方法を学ぶことができます。

## ハウツー

まずは、Gleamでテキストファイルを読み込む方法を見てみましょう。Gleamでは、```File.read("path/to/file.txt")```を使用して、ファイルを読み込むことができます。その後、読み込んだデータを変数に代入し、出力することができます。以下に例を示します。

```Gleam
// テキストファイルを読み込み、変数に代入する
let file_data = File.read("path/to/file.txt")

// 変数に代入したデータを出力する
IO.println(file_data)
```

上記のコードを実行すると、コンソールにファイルの中身が表示されます。ただし、読み込んだデータはバイナリ形式であるため、文字列に変換する必要があります。そこで、```to_string```関数を使用して、バイナリデータを文字列に変換することができます。以下に例を示します。

```Gleam
// テキストファイルを読み込み、変数に代入する
let file_data = File.read("path/to/file.txt")

// 変数に代入したデータを文字列に変換し、出力する
IO.println(to_string(file_data))
```

このように、テキストファイルの読み込みは非常に簡単です。しかし、もっと深く知りたい方もいるかもしれません。そこで、次のセクションでは、テキストファイルをより詳しく掘り下げてみましょう。

## ディープダイブ

テキストファイルを読み込む際に、様々なオプションを設定することができます。たとえば、文字コードや改行コードを指定することができます。また、ファイルの一部分だけを読み込むこともできます。詳しくは[Gleamの公式ドキュメント](https://gleam.run/documentation/standard-library#text-file)をご覧ください。さらに、ファイルを書き込む方法も[Gleamの公式ドキュメント](https://gleam.run/documentation/standard-library#files)で確認することができます。

## See Also

- [Gleamの公式ドキュメント](https://gleam.run/documentation)
- [Gleamのソースコード](https://github.com/gleam-lang/gleam)