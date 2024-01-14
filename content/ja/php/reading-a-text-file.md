---
title:                "PHP: テキストファイルの読み込み"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜこの記事を読むべきか

テキストファイルを読み込むことは、多くのプログラマーにとって基本的なスキルです。例えば、データベースやファイルを使用する前に、データをテキストファイルで書き込んだり、バックアップしたりする必要があるからです。この記事では、PHPを使用してテキストファイルを読み込む方法を紹介します。

## 使い方

まずは、PHPのファイルを開いて、新しい変数を作成しましょう。

```
<?php
$file = "example.txt";
```

次に、```file_get_contents()```関数を使用して、テキストファイルを読み込みます。

```
$content = file_get_contents($file);
```

上記のコードを実行すると、指定したファイルの内容が変数```$content```に代入されます。これで、テキストファイルを読み込むことができました。

もし、テキストファイルの内容を一行ずつ読み込みたい場合は、```file()```関数を使用します。

```
$file_lines = file($file);
```

上記のコードを実行すると、テキストファイルの各行が配列に代入されます。この配列をループ処理して、各行の内容を表示することができます。

## 詳細を深く掘り下げる

テキストファイルを読み込む際には、いくつかの注意点があります。例えば、日本語を含んだファイルを読み込む場合は、```mb_convert_encoding()```関数を使用して文字コードを変換する必要があります。また、テキストファイル以外のファイルを読み込む場合は、```fread()```関数を使用する必要があります。

また、ファイルを開くときは、ファイル名やパスに注意する必要があります。読み込むファイルが現在のディレクトリに存在しない場合は、フルパスを指定する必要があります。

## 参考になるリンク

この記事では、PHPを使用してテキストファイルを読み込む方法を紹介しましたが、他にもさまざまな方法があります。さらに詳しく学びたい方は、以下のリンクを参考にしてください。

- [PHP: file_get_contents - Manual](https://www.php.net/manual/en/function.file-get-contents.php)
- [PHP: file - Manual](https://www.php.net/manual/en/function.file.php)
- [PHP: fread - Manual](https://www.php.net/manual/en/function.fread.php)
- [PHP: mb_convert_encoding - Manual](https://www.php.net/manual/en/function.mb-convert-encoding.php)

## 関連リンク

- [PHPでテキストファイルを書き込む方法](https://example.com/how-to-write-text-file-with-php)
- [PHPでファイルをバックアップする方法](https://example.com/how-to-backup-file-with-php)
- [PHPでCSVファイルを読み込む方法](https://example.com/how-to-read-csv-file-with-php)